require 'ol'
require 'core_ext'
require 'menu'

require 'launcher'

class XikiCommand

  # Called by the 'xiki' shell command
  def self.run

    Xiki.init

    return self.usage if ARGV.empty?

    flags, path = ARGV.partition{|o| o =~ /^-/}

    path = path.join ' '

    # If -p, just prompt user to type a menu name
    if flags.member?("-e")
      return self.emacs path
    end

    # Invoke menu...

    puts Menu[path]

  end

  def self.usage
    %`
    > Summary
    This command runs xiki menus, which are simple files found
    in ~/menus/.

    > Examples
    Call 'ip' menu
    % xiki ip

    Show all menus
    % xiki all

    Open 'ip' menu in emacs
    % xiki -e ip

    Run under current dir
    % xiki -e @git

    bring up in emacs
    aoeu - see:
    | % xiki __
    % xiki -e

    > TODO
    - show all menus?
      - Or just show how to show all
        xiki all
    `.unindent
  end

  #
  # Tells emacs to open and display menu.
  #
  def self.emacs menu

    # Bring emacs to front

    `open "/Applications/Aquamacs Emacs.app"`
    # `open "/Applications/emacs.app"`   # If you're not using Aquamacs

    ruby = %`Menu.external \\"#{menu}\\"`
    ruby << %`, :dir=>\\"#{Dir.pwd}\\"` if menu =~ /^@/

      command = %`emacsclient -n -e '(el4r-ruby-eval "#{ruby}")'`
    `#{command}`

    nil
  end

end

