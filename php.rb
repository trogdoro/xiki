class Php
  def self.run
    # Get block contents
    txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

    result = self.run_internal txt
    # Insert result at end of block
    orig = Location.new
    View.cursor = right
    Line.to_left
    View.insert result.gsub(/^/, '  ')+"\n"
    orig.go
  end

  def self.run_internal txt
    # Write to temp file
    File.open("/tmp/tmp.php", "w") { |f| f << "<?\n#{txt}\n?>\n" }
    # Call js
    Console.run "php -f /tmp/tmp.php", :sync=>true
  end

end
