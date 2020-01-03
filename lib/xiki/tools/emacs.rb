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

    def self.v22
      self.version_number(:major=>1) == "22"
    end

    def self.version_number options={}
      return $el.emacs_version[/[\d]+/] if options[:major]
      $el.emacs_version[/[\d.]+/]
    end

    def self.version
      Tree.quote $el.emacs_version
    end

    def self.info_pages name
      View.to_upper
      $el.info "(emacs)#{name}"
      nil
    end

    # Emacs.regexp_quote "hi?"
    #   "hi\?"
    def self.regexp_quote txt   # Escape regex
      $el.regexp_quote txt
    end

  end
end
