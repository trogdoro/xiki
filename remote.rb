require 'rubygems'
gem 'net-ssh'
require 'net/ssh'
require 'net/sftp'
require 'timeout'

class Remote
  extend ElMixin

  @@temp_dir = "/tmp/remote_rb"

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
          return self.save_file(path, @@connections["#{user}@#{server}:#{port}"])
        else
          return puts("- Action '#{action}' unimplemented!")
        end

      else  # If just path, append it to the end
        path << "#{path_append.join('')}"  # append to path
      end
    end

    timeout(6) do
      if path =~ /\/$/   # If a dir
        out = @@connections["#{user}@#{server}:#{port}"].exec!("ls -p #{path}")
        # Weed out #...#
        out = out.grep(/^[^#]+$/).join("")
        # Change @ to /
        out.gsub!(/@/, '/')

        out = self.sort(out)

        puts out

      else   # If a file

        Dir.mkdir @@temp_dir unless File.exists? @@temp_dir
        local_path = self.calculate_local_path path
        #local_path = "#{@@temp_dir}/#{path.gsub('/', '-')[1..-1]}"

        was_open = Files.open? local_path

        # Download if not open already
        unless was_open
          ftp = @@connections["#{user}@#{server}:#{port}"].sftp.connect
          ftp.download!(path, local_path)
        end

        View.to_after_bar
        View.open local_path
        puts "- save" unless was_open

        # TODO save timestamp in buffer var
        # - Use it to determine whether file changed when saving

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

  def self.save_file path, connection
    local_path = self.calculate_local_path path
    # If not open, print error
    return puts("- File no longer open!") unless Files.open? local_path

    begin
      # Do save
      sftp = connection.sftp.connect

      sftp.upload!(local_path, path)
      puts "- success!"
    rescue Exception => e
      puts "- error: #{e.message}"
    end

  end

  def self.calculate_local_path path
    "#{@@temp_dir}/#{path.gsub('/', '-')[1..-1]}"
  end

end
