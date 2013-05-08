class Des
  def self.menu *args

    options = yield
    options[:no_slash] = 1
    Line.delete

    txt = %`
      describe __, "#__" do
        it "__" do
          1.should == 2
        end
      end
      `.unindent

    View.insert txt.gsub(/^/, '  '), :dont_move=>1
    Move.to_end

    nil
  end
end
