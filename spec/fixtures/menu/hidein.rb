class Hidein

  MENU_HIDDEN = "
    - .bar/
    "
  # Not sure if this matters
  def self.menu *args
    "
    - foo #{args}
    | Also try the hidden 'bar' item
    "
  end
  def self.bar *args
    "Rose-anne"
  end
end
