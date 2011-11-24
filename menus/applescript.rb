class Applescript
  def self.menu *command

    # If no arg, prompt to type something

    return View.prompt "Type something to run in applescript" if command.empty?

    command = command.join('/')
    command = Xiki.branch if command =~ /^\|/
    $el.do_applescript command

    nil
  end
end
