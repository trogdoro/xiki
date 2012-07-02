class Python
  def self.menu
    "
    > Pass python code to run
    | print('hey')
    "
  end

  def self.menu_after txt, *args
    # Do nothing if .menu created output
    return nil if txt

    txt = ENV['txt']
    return nil if txt.nil?

    txt = Python.run_internal txt
    txt
  end

  def self.run_block
    # Get block contents
    txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

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
end
