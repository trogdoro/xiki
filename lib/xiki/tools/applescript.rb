module Xiki
  class Applescript

    #   def self.menu *command
    def self.menu
      %`
      > Pass in applescript
      | tell application "iTunes" to playpause
      - .api/
        | Run applescript
        @Applescript.run 'tell application "iTunes" to playpause'
        |
        | Shortcut to wrap "tell" in block
        @Applescript.run "playpause", :app=>"iTunes"
        @Applescript.run "get the name of every track of library playlist 1 as string", :app=>"iTunes", :delimiter=>"|"
      `
    end

    def self.menu_after output, *args

      return output if output   # If menu handled it, just return

      command = ENV['txt']
      result = self.run command

      ".flash - ran it!"
    end

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

        return $el.do_applescript txt
      end

      $el.do_applescript "#{extra}#{command}"
    end
  end
end
