class Cla
  def self.menu *args
    Line.delete

    txt = "
      class 
        def self.hi
          \"hi\"
        end
      end
      ".unindent

    View.insert txt, :dont_move=>1
    Move.to_end
    ControlLock.disable

    nil
  end
end
