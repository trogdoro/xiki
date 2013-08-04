require 'xiki/core/ol'

require "active_support"
require "active_support/time"

require 'xiki/core/core_ext'
require 'xiki/core/menu'
require 'xiki/core/launcher'

is_windows = (RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/)

if is_windows
  require 'win32/pipe'
  include Win32
end

Xiki.init

# Make named pipes for input and output

class XikiProcess

  def self.run

    is_windows?Pipe::Server.new('xikirequest'):open('/tmp/xikirequest', 'r+') do |f|
      if is_windows then f.connect end
      is_windows?Pipe::Client.new('xikiresponse'):open('/tmp/xikiresponse', 'w+') do |response|
        loop do

          # Read request...

          path = XikiCommand.pop_initial_request || f.gets
          path.strip!

          options = {}

          # If line is "@options/...", parse it and read path...

          if path =~ /^@options\/(.+)/
            options = JSON[$1]

            # Turn keys into symbols
            options = options.reduce({}){ |acc, o| acc[o[0].to_sym] = o[1]; acc }

            path = is_windows?f.read:f.gets
            path.strip!
          end

          # Invoke menu and send response...

          menu_output =
            begin
              Xiki[path, options]
            rescue Exception=>e
              "#{e.message}\n#{e.backtrace.join("\n")}"
            end

          if menu_output.nil?
            menu_output = ""
          end

          menu_output = menu_output.to_s

          menu_output.gsub! "\n", "\036"   # Escape linebreaks as 036 char (record separator)
          if is_windows
            response.write menu_output
            response.close
          else
            response.puts menu_output
            response.flush
          end
        end
      end
    end

  rescue Exception=>e
    puts "#{e.message}\n#{e.backtrace}"
  end
end

XikiProcess.run
