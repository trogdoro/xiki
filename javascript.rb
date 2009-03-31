class Javascript
  def self.run
    # Get block contents
    txt = Notes.block("^|")

    txt << "
      function p(txt) {
        print(txt);
      }";

    result = self.run_internal txt
    # Insert result at end of block
    orig = Location.new
    Search.forward "^|"
    Line.to_left
    View.insert result
    orig.go
  end

  def self.run_internal txt
    # Write to temp file
    File.open("/tmp/tmp.js", "w") { |f| f << txt }
    # Call js
    Console.run "js /tmp/tmp.js", :sync=>true
  end

  def self.launch
    line = Line.without_label
    result = self.run_internal line
    FileTree.insert_under result, :escape=>''
  end
end
