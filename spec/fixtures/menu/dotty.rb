class Dotty
  MENU =
    "
    > From MENU constant
    - .methane/
    - .methlab/
      - menu param/
        - another
    - .nested dots/
      - .dotted/
        - plain
    - sub dots/
      - .dotted/
        - plain
    - normal/
    - .params with dots/
    "
  def self.methane
    "- hi from self.methane"
  end
  def self.methlab *args
    "- hi from self.methlab: #{args.inspect}"
  end
  def self.params_with_dots *args
    return ".normalparam" if args.blank?
    "you passed me: #{args.inspect}"
  end

  def self.dotted *args
    "dot: #{args}"
  end


  def self.menu *args
    "- in last case, fall back to .menu"
  end
end
