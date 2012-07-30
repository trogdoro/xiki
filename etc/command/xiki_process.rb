# Don't hard-code, generate with script? - or use __file path xiki env var!
require '/projects/xiki/lib/ol'
require 'core_ext'

require 'menu'

require 'launcher'

Xiki.init

# Make named pipes for input and output

class XikiProcess

  def self.run

    open('/tmp/xikirequest', 'r+') do |f|

      open('/tmp/xikiresponse', 'w+') do |response|
        loop do

          # Read request...

          path = XikiCommand.pop_initial_request || f.gets
          path.strip!

          # Invoke menu and send response...

          menu_output =
            begin
              Menu[path]
            rescue Exception=>e
              "#{e.message}\n#{e.backtrace.join("\n")}"
            end

          if menu_output.nil?
            menu_output = ""
          end

          menu_output.gsub! "\n", "\036"   # Escape linebreaks as 036 char (record separator)
          response.puts menu_output
          response.flush
        end
      end
    end

  rescue Exception=>e
    puts "#{e.message}\n#{e.backtrace}"
  end
end

XikiProcess.run
