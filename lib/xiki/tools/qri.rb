module Xiki
  class Qri
    def self.menu *txt
      txt = txt.any? ? txt.join('/') : nil

      return ".prompt Type a ruby method or class" if ! txt

      # Text passed, so look it up
      Tree.quote `qri -f plain "#{txt}"`

    end
  end
end
