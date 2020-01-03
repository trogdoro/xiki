module Menu
  class Applescript

    MENU = %`
      > Pass in applescript to run
      | tell application "iTunes" to playpause
      - docs/
        - examples/
          - one line/
            =applescript/iTunes/playpause
          - multiple lines/
            =applescript/
              | tell application "iTunes"
              |   playpause
              | end
          - terminal/
            =applescript/Terminal/do script "pwd" in window 1
        - api/
          - run applescript/
            =code/
              | Applescript.run 'tell application "iTunes" to playpause'
          - shortcut to wrap 'tell' in block/
            =code/
              | Applescript.run "playpause", :app=>"iTunes"
              | Applescript.run "get the name of every track of library playlist 1 as string", :app=>"iTunes", :delimiter=>"|"
      `

    def self.menu_after output, *args

      return output if output   # MENU handled it, so don't interfere

      command = args[0]

      # /Application/script, so send specific app...

      app, command = args if args[1]

      # If only 1 line, remove colon
      command.sub! /^: /, '' if command !~ /\n/

      # /script, so run it...

      result = Xiki::Applescript.run command, :app=>app

      "<*"
    end

  end
end

