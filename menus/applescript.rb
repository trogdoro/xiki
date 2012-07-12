class Applescript

  #   def self.menu *command
  def self.menu
    %`
    > Pass in applescript
    | tell application "iTunes" to playpause
    - .api/
      | Run applescript
      @ Applescript.run 'tell application "iTunes" to playpause'
      |
      | Shortcut to wrap "tell" in block
      @ Applescript.run "iTunes", "playpause"
    - .docs/
      > Examples
      | Applescript.run "iTunes", "playpause"
    `
  end

  def self.menu_after output, *args

    return output if output   # If menu handled it, just return

    command = ENV['txt']
    result = self.run command

    ".flash - ran it!"
  end

  def self.run txt, command=nil

    # If 2nd arg passed, treat first as app
    if command.present?

      txt = "
        tell application \"#{txt}\"
          #{command}
        end tell
        ".unindent

      return $el.do_applescript txt
    end

    $el.do_applescript txt
  end
end
