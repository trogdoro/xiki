class Def
  def self.menu *args
    Line.delete

    txt = %`
      def self.
        "hi"
      end
      `.unindent

    View.insert txt.gsub(/^/, '  '), :dont_move=>1
    Move.to_end
    #     ControlLock.disable

    nil
  end
end
