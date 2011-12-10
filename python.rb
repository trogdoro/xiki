class Python
  def self.menu *args
    txt = ENV['txt']

    self.run_internal txt
  end

  def self.run
    # Get block contents
    txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

    #     txt << "
    #       function p(txt) {
    #         print(txt);
    #       }";

    result = self.run_internal txt
    # Insert result at end of block
    orig = Location.new
    View.cursor = right
    Line.to_left
    View.insert result.gsub(/^/, '  ')
    orig.go
  end

  def self.run_internal txt
    # Write to temp file
    File.open("/tmp/tmp.py", "w") { |f| f << txt }
    # Call js
    Console.run "python /tmp/tmp.py", :sync=>true
  end

  #   def self.launch
  #     line = Line.without_label
  #     result = self.run_internal line
  #     FileTree.under result, :escape=>''
  #   end
end
