module Menu
  class Help

    def self.menu_after txt, *args
      options = yield
      options[:hotkey] = nil
      txt
    end

    MENU = %`
      | Welcome to xsh! Here are some places to look for help:
      |
      | Type "xsh --help" back in your normal shell, or expand
      | it right here:
      = $ xsh --help

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

      | See
      << help topics/
      << faq/
      << tutorial/
      << keys/
      << conf/
    `
  end
end
