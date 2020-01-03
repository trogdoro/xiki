module Xiki
  class Applescript

    #
    # Applescript.run 'tell application "iTunes" to playpause'
    # Applescript.run "playpause", :app=>"iTunes"
    # Applescript.run "get the name of every track of library playlist 1 as string", :app=>"iTunes", :delimiter=>"|"
    #
    def self.run command, options={}

      # If 2nd arg is string, assume "app", "command"
      if options.is_a?(String)
        options, command = {:app=>command}, options
      end

      extra = "set Applescript's text item delimiters to \"#{options[:delimiter]}\"\n" if options[:delimiter]

      # If 2nd arg passed, treat first as app
      if options[:app]

        txt = "
          tell application \"#{options[:app]}\"
            #{extra}#{command}
          end tell
          ".unindent

        return self.do_applescript txt
      end

      self.do_applescript "#{extra}#{command}"
    end

    def self.do_applescript txt
      txt = File.open("/tmp/applescript.txt", "w") { |f| f << txt }
      Shell.sync "osascript /tmp/applescript.txt"
    end



  end
end
