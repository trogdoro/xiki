class Emacs
  def self.menu
    "
    - .info/
      - Keys/
      - Prefix Keymaps/
      - Keymaps/
      - Keymaps/
      - Prefix Keymaps/
    "
  end

  def self.info name
    View.to_upper
    $el.info "(emacs)#{name}"
    nil
  end
end
