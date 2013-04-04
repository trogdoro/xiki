class Hideout
  # Not sure if this matters
  MENU = "
    - fooc/
      > ooooo
    | Also try the hidden 'bar' item
    "
  MENU_HIDDEN = "
    - .bar/
    "
  def self.bar *args
    "Roseanne"
  end
end
