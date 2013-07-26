module MENU
  class Applescript

    MENU = %`
      > Pass in applescript to run
      | tell application "iTunes" to playpause
      - api/
        | Run applescript
        @! Applescript.run 'tell application "iTunes" to playpause'
        |
        | Shortcut to wrap "tell" in block
        @! Applescript.run "playpause", :app=>"iTunes"
        @! Applescript.run "get the name of every track of library playlist 1 as string", :app=>"iTunes", :delimiter=>"|"
      `

    def self.menu_after output, *txt

      return output if output   # If menu handled it, just return

      return "@beg/quoted/" if txt[0] !~ /\n/
      command = txt[0]
      command.gsub! /^\| ?/, ''
      result = Xiki::Applescript.run command

      "@flash/- ran it!"
    end

  end
end

# Change this code to something useful.
# a = "la"
# a * 3
# Tree.children Xiki::Applescript::MENU, args

