class Iterm
  def self.menu *command

    # If no arg, prompt to type something

    return View.prompt "Type something to run in iTerm" if command.empty?

    command = command.join('/')

    return self.run("activate") if command == "start"
    return self.run("quit") if command == "quit"

    self.run :command=>command.sub(/^\| /, '')
  end

  def self.run action
    if action.is_a? String
      $el.do_applescript %`
        #{action} application "iTerm"
        `
      return nil
    end

    $el.do_applescript %`
      tell application "iTerm"
        tell the first terminal
          tell the last session
            write text "#{action[:command]}"
          end tell
        end tell
      end tell
      `
    nil
  end
end
