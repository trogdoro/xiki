require 'daemons'

class XikiProcess

  def self.running?
    files = Daemons::PidFile.find_files( "/tmp", "xiki_process" )
    raise "Multiple PID files found for xiki_process!" if files.length > 1
    return false if files.length == 0
    pid_file = Daemons::PidFile.existing( files[0] )
    Daemons::Pid.running? pid_file.pid() 
  end

  def self.start_daemon
    xiki_dir = File.expand_path "#{File.dirname(__FILE__)}/../.."
    begin
      pid_orig = Process.pid
      Daemons.run(
        "#{xiki_dir}/bin/xiki_process",
        :ARGV => ['start'],
        :monitor => false,
        :multiple => false,
        :dir_mode => :normal,
        :dir => "/tmp/",
        :log_dir => "/tmp/",
        :log_output => true
      )
    rescue SystemExit=>e
      return
    rescue Exception=>e
      puts "- service couldn't start!:#{e.message}\n#{e.backtrace.join("\n")}\n\n"
    end
  end

  def self.run

    # We require these here so as not to slow down processes just wanting to do andilliary process stuff
    require 'socket'
    require 'xiki/core_ext'
    require 'xiki/menu'
    require 'xiki/launcher'

    Xiki.init

    server = TCPServer.open(22112)
    loop do
      Thread.start(server.accept) do |client|
          path = client.gets
          path.strip!

          menu_output =
            begin
              Menu[path]
            rescue Exception=>e
              "#{e.message}\n#{e.backtrace.join("\n")}"
            end

          menu_output = "" if menu_output.nil?

          menu_output.gsub! "\n", "\036"   # Escape linebreaks as 036 char (record separator)
          client.puts menu_output
      end
    end
  rescue Exception=>e
    puts "#{e.message}\n#{e.backtrace}"
  end
end