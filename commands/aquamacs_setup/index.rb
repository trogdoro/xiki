module Xiki::Menu
  class AquamacsSetup

    def self.menu_after output, *args
      if Environment.os != "osx"
        return "
          Aquamacs only works on OSX.
          Try regurlar Emacs:
          @emacs setup/
          "
      end

      nil
    end
  end
end
