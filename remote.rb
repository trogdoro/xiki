require 'rubygems'
gem 'net-ssh'
require 'net/ssh'
require 'net/sftp'
require 'timeout'

class Remote
  extend ElMixin

  @@connections = {}
  @@default_dirs ||= []

  def self.menu
    @@default_dirs.each do |d|
      puts ".dir '#{d}'"
    end
    puts ".dir 'user@foo.com:22/tmp'"
  end

  # default_dirs attr
  def self.default_dirs= to;  @@default_dirs = to;  end
  def self.default_dirs;  @@default_dirs;  end

  def self.dir root, *path_append
    user, server, port, path = self.split_root(root)

    @@connections["#{user}@#{server}:#{port}"] ||= self.create_connection(user, server, port)

    # Add slash to path if none there
    path << "/" unless path =~ /\/$/

    path_passed = path_append.size > 0

    if path_passed  # If anything in array
      if Line.is_bullet?  # If it was a bullet, do the action
        # Pull off action
        action = path_append.pop
        if action == "save"
          path << "#{path_append.join('')}"  # append to path
          return self.save_file(path, @@connections["#{user}@#{server}:#{port}"], remote_buffer_name(server, path))
        else
          return puts "- Action '#{action}' unimplemented!"
        end

      else  # If just path, append it to the end
        path << "#{path_append.join('')}"  # append to path
      end
    end

    timeout(6) do
      # If a dir
      if path =~ /\/$/
        out = @@connections["#{user}@#{server}:#{port}"].exec!("ls -p #{path}")
        # Weed out #...#
        out = out.grep(/^[^#]+$/).join("")
        # Change @ to /
        out.gsub!(/@/, '/')

        out = self.sort(out)

        puts out

      # If a file
      else
        out = @@connections["#{user}@#{server}:#{port}"].exec!("cat #{path}")
        out.gsub!(/\C-m/, '')

        # Get buffer name
        buffer = remote_buffer_name(server, path)
        was_open = View.buffer_open? buffer
        View.to_after_bar
        View.to_buffer buffer

        # If open already
        if was_open
          if View.txt == out  # Compare buffer to remote
            # If same, do nothing
          else  # If different
            beep
            puts "- warning: Version on server is different than buffer"
            # TODO: Save current window configuration
            # Put server version in a different dir
            # Diff buffers
          end
        # Otherwise, open and insert
        else
          puts "- save"
          Notes.mode
          insert out
          View.to_top
        end

      end
    end
  end

  def self.create_connection(user, server, port)
    begin
      timeout(6) do
        Net::SSH.start(server, user, :port => port.to_i, :paranoid => false)
      end
    rescue Exception => e
      puts "Timed out: #{e.message}"
    end
  end

  # Splits name@server:port/path
  def self.split_root root
    # Append / at end if no / exists
    root = root.dup
    root << '/' unless root =~ /\/$/
    root.match(/^(.+?)@(.+?):(.+?)(\/.*?)\/?$/)[1..4]
  end

  def self.sort lines
    l = lines.split("\n")
    l.sort!{|a,b| a.sub(/(.+)\//, "/\1") <=> b.sub(/(.+)\//, "/\1")}
    l.join("\n") + "\n"
  end

  def self.remote_buffer_name(server, path)
    dir, file = path.match(/(.+\/)(.+)/)[1..2]
    "*remote #{file} (#{server}:#{dir})"
  end

  def self.save_file(path, connection, buffer)
    # If not open, print error
    return puts "- File no longer open!" unless View.buffer_open? buffer


    # Get text from buffer
    View.to_after_bar
    View.to_buffer buffer
    txt = View.txt

    Effects.blink :what => :all

    begin
      # Do save
      sftp = connection.sftp.connect


      # open and write to a pseudo-IO for a remote file
      sftp.file.open(path, "w") do |f|
        result = f.puts txt
        puts "- success!"
      end

    rescue Exception => e
      puts "- error: #{e.message}"
    end

  end
end
