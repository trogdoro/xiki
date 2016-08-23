class Def
  def self.menu *args

    options = yield
    options[:no_slash] = 1
    Line.delete

    txt = %`
      def self.
        "hi"
      end
      `.unindent

    View.insert txt.gsub(/^/, '  '), :dont_move=>1
    Move.to_end

    ""
  end
end
