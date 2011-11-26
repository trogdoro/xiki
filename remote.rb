gem 'net-ssh'
require 'net/ssh'
require 'net/sftp'
require 'timeout'
require 'ol'

class Remote
  extend ElMixin

  @@temp_dir = "/tmp/remote_rb"

  @@connections = {}
  @@default_dirs ||= []

  def self.menu
    out = ""
    @@default_dirs.each do |d|
      out << "- /#{d}/\n"
    end
    out << "- @/user@foo.com/tmp/\n"
    out
  end

  # default_dirs attr
  def self.default_dirs= to;  @@default_dirs = to;  end
  def self.default_dirs;  @@default_dirs;  end

  # Called when dir or file is launched
  def self.file_contents whole_path
    user, server, port, path = self.split_root(whole_path)
    connection = self.connection whole_path
    connection.sftp.download!(path)
  end

  def self.dir root, *path_append

    connection = self.connection root

    user, server, port, path = self.split_root(root)

    # Add slash to path if none there
    path << "/" unless path =~ /\/$/

    path_passed = path_append.size > 0

    if path_passed  # If anything in array
      path << "#{path_append.join('')}"  # append to path
      path.sub! /^\/\//, '/'
    end

    timeout(6) do
      if path =~ /\/$/   # If a dir
        out = connection.exec!("ls -pa #{path}")
        out ||= ""
        out = out.grep(/^[^#]+$/).join("")   # Weed out #...#
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

  def self.command root #, *path_append

    the_command = root.last[/! ?(.+)/, 1]
    # Pull off command
    while(root.last =~ /^!/) do   # Remove all !foo lines from root
      root.pop
    end
    root = root.join('')
    connection = self.connection root

    user, server, port, path = self.split_root(root)

    path << "/" unless path =~ /\/$/   # Add slash to path if none there

    timeout(6) do
      out = connection.exec!("cd \"#{path}\" && #{the_command}")
      #       out = connection.exec!("cd \"#{path}\"; #{the_command}")
      out ||= ""

      Tree.under out, :escape=>'| '
    end
  end

  def self.connection root
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
      puts "Timed out: #{e.message}"
    end
  end


  def self.split_root root   # Splits name@server:port/path
    root = root.dup   # Append / at end if no / exists
    root << "/" unless root =~ /\/$/

    user, server_port, path = root.match(/^(.+?)@(.+?)(\/.*?)\/?$/)[1..3]

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

end

Keys.do_as_remote do
  Remote.save_file
end


# Remote.init
