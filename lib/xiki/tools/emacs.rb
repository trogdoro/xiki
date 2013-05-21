module Xiki
  class Emacs
    def self.menu
      "
      - .version/
      - .info pages/
        - Keys/
        - Prefix Keymaps/
        - Keymaps/
        - Prefix Keymaps/
      "
    end

    def self.version
      Tree.quote $el.emacs_version
    end

    def self.info_pages name
      View.to_upper
      $el.info "(emacs)#{name}"
      nil
    end
  end
end
