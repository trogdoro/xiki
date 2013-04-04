class Constable
  MENU =
    "
    > From MENU constante
    - con1/
      - con2/
    - .conm/
    - conb/
      - whatever/
    "
  def self.conm
    "- hi from self.conm"
  end
  def self.menu *args
    "- hi from self.menu"
  end
end
