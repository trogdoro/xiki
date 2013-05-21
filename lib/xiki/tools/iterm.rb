class Iterm
  def self.menu *args

    # If no arg, prompt to type something

    return View.prompt "Type something to run in iTerm" if args.empty?

    txt = args.join('/')

    return self.command("activate") if txt == "start"
    return self.command("quit") if txt == "quit"

    self.run txt.sub(/^\| /, '')
  end

  def self.command txt
    $el.do_applescript %`
      #{txt} application "iTerm"
      `
    return nil
  end

  def self.run txt, options={}
    $el.do_applescript %`
      tell application "iTerm"
        #{options[:activate] ? 'activate' : ''}
        tell the first terminal
          tell the last session
            write text "#{txt}"
          end tell
        end tell
      end tell
      `
    nil
  end
end
