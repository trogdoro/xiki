require 'timeout'
require 'xiki/core/ol'

module Xiki
  class Remote

    @@temp_dir = "/tmp/remote_rb"

    @@connections = {}

    def self.menu
      "
      - docs/
        > Summary
        | You can browse files and run commands on remote servers.
        @ /user@foo.com/tmp/
      << see) @servers/
      "
    end

    # Called when dir or file is launched
    def self.file_contents whole_path
      user, server, port, path = self.split_root(whole_path)
      connection = self.connection whole_path
      connection.sftp.download!(path)
    end

    def self.dir root, *path_append

      connection = self.connection root

      user, server, port, path = self.split_root(root)

      path_passed = path_append.size > 0

      if path_passed  # If anything in array
        path << "#{path_append.join('')}"  # append to path
        path.sub! /^\/\//, '/'
      end

      timeout(15) do
        if path =~ /\/$/   # If a dir
          path << "/" unless path =~ /\/$/
          out = connection.exec!("ls -pa #{path}")
          out ||= ""
          out = out.split("\n").grep(/^[^#]+$/).join("\n")   # Weed out #...#
          out.gsub!(/@/, '/')   # Change @ to /

          # Get rid of . and ..
          out = out.split("\n").select{|o| o !~ /^\.+\/$/}.join("\n")+"\n"
          self.sort(out)

        else   # If a file

          Dir.mkdir @@temp_dir unless File.exists? @@temp_dir
          local_path = self.calculate_local_path path, server

          was_open = Files.open? local_path

          # Download if not open already
          unless was_open
            begin
              connection.sftp.download!(path, local_path)
            rescue Exception=>e
              # If doesn't exist, we'll just create
            end
          end

          View.to_after_bar
          View.open local_path

          # TODO: save root path as var in buffer
          $el.make_local_variable :remote_rb_server_root
          server_root = "/#{user}@#{server}#{port ? ":#{port}" : ""}/"
          $el.elvar.remote_rb_server_root = server_root

          # TODO save timestamp in buffer var
          # - Use it to determine whether file changed when saving

        end
      end
    end

    # Handles only async?
    def self.command root, command #, *path_append

      connection = self.connection root

      user, server, port, path = self.split_root(root)

      timeout(6) do
        out = connection.exec!("cd \"#{path}\" && #{command}")
        out ||= ""

        out
      end
    end

    def self.connections
      @@connections
    end

    def self.connection root
      require 'net/ssh'
      require 'net/scp'
      user, server, port, path = self.split_root root
      address = "#{user}@#{server}:#{port}"
      @@connections[address] ||= self.new_connection user, server, port.to_i
      @@connections[address]
    end

    def self.new_connection user, server, port
      begin
        timeout(6) do
          Net::SSH.start(server, user, :port => port.to_i, :paranoid => false)
        end
      rescue Exception => e
        raise "Timed out: #{e.message}"
      end
    end

    def self.split_root root   # Splits name@server:port/path
      root = root.dup   # Append / at end if no / exists

      user, server_port, path = root.match(/^(.+?)@([^\/]+)(.*)/)[1..3]

      if(server_port =~ /(.+?):(.+)/)
        server, port = $1, $2
      else
        server, port = server_port, "22"
      end

      user.sub! /^\//, ''
      [user, server, port, path]
    end

    def self.sort lines
      l = lines.split("\n")
      l.sort!{|a,b| a.sub(/(.+)\//, "\\1") <=> b.sub(/(.+)\//, "\\1")}
    end

    def self.remote_buffer_name(server, path)
      dir, file = path.match(/(.+\/)(.+)/)[1..2]
      "*remote #{file} (#{server}:#{dir})"
    end

    def self.save_file
      local_path = View.file
      # Error out if not in right place
      if local_path !~ /^#{@@temp_dir}/
        View.beep
        return View.message("This isn't a file the Remote code_tree retrieved")
      end

      # Save if modified
      $el.save_buffer if $el.buffer_modified_p

      remote_path = $el.elvar.remote_rb_server_root


      # Convert to path
      remote_path = self.calculate_remote_path local_path
      begin   # Do save
        connection = self.connection $el.elvar.remote_rb_server_root
        connection.sftp.upload!(local_path, remote_path)
        View.message "successfully saved remotely!"
      rescue Exception => e
        View.message "- error: #{e.message}"
      end

    end

    def self.calculate_local_path path, server
      "#{@@temp_dir}/#{server},#{path.gsub('/', ',')[1..-1]}"
    end

    def self.calculate_remote_path path
      path.gsub(/^#{@@temp_dir}\/.+?,/, '/').gsub(',', '/')
    end

    def self.init
      # TODO remove this
    end

    def self.remote_files_in_dir dir
      txt = self.dir(dir)
      return txt if txt.is_a? String   # If it's a file

      txt.map!{|i| "#{dir}#{i}"}
      [txt.select{|i| i =~ /\/$/}.map{|i| i.sub(/\/$/, '')}, txt.select{|i| i !~ /\/$/}]
    end

    def self.remote_file_contents file
      path, file = file.match(/(.+\/)(.+)/)[1..2]
      self.dir path, file   # Delegate to Remote.dir
    end

    # Handles if $... or %...
    def self.expand_command path, options

      # Not $... or %..., so we don't handle it...

      path = Path.split path
      return if path[-1] !~ /^[$%] /

      shell_prompt, command = /(.).(.+)/.match(path.pop)[1..2]

      # Get rid of any other nested
      path.pop while(path.last =~ /^[$%] /)
      path = path.join('/')

      # $, so sync command...

      return Remote.command path, command if shell_prompt == "$"

      # %, so async command...

      view_orig = View.name

      Shell.to_shell_buffer path, :cd_and_wait=>true

      View.insert command
      Shell.enter

      View.to_buffer view_orig if View.buffer_visible? view_orig

      ""   # Handled it

    end

    def self.expand path, options

      # $ foo, so run as a shell command...

      txt = self.expand_command path, options
      if txt
        return Tree.quote(txt) if txt.any?
        return ""   # Async returns blank string so don't quote
      end

      # It's a dir (or file?)...

      dirs, files = self.remote_files_in_dir path

      return if dirs.is_a? String

      # If empty, say so...

      return "<! dir is empty!" if files.empty? && dirs.empty?

      indent = "#{Line.indent}  "

      # Change path to proper indent
      dirs.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1/")}
      files.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1")}

      Line.next
      left = View.cursor

      both = dirs + files

      View.insert(both.join("\n") + "\n")
      right = View.cursor
      View.cursor = left
      Line.to_beginning
      Tree.filter(:left=>left, :right=>right, :always_search=>true)

    end
  end

end
