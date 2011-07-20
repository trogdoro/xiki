class CoffeeScript
  def self.run
    # Get block contents
    txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

    result = self.run_internal txt
    # Insert result at end of block
    orig = Location.new
    View.cursor = right
    Line.to_left
    View.insert result.gsub(/^/, '  ')+"\n\n"
    orig.go
  end

  def self.run_internal txt
    # Write to temp file
    File.open("/tmp/tmp.coffee", "w") { |f| f << txt }
    # Call js
    result = Console.run "coffee -pc /tmp/tmp.coffee", :sync=>true
    result.split("\n")[1..-2].join("\n")
  end

end

Keys.do_as_coffee { CoffeeScript.run }
