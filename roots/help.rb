module Menu
  class Help

    def self.menu_after txt, *args
      options = yield
      options[:hotkey] = nil
      txt
    end

    MENU = %`
      | Welcome to xsh (Xiki Shell)!
      |
      | For the tutorial, type Ctrl+Q then "xsh --tutorial".
      |
      | For help with command line args, type Ctrl+Q to quit, then type
      | "xsh --help" back in your normal shell. Or expand it right here
      | (move your arrow keys to the following line and press Ctrl+O):
      |
      $ xsh --help
      |
      | Connecting with people
      - chat room/
        | Jump into the Xiki chat room to get help, chat about,
        | Xiki or propose a command!
        |
        = http://webchat.freenode.net/?channels=xiki
      - mailing list/
        | Join the mailing list to get help via email.
        |
        = http://groups.google.com/group/xiki/

    `
  end
end
