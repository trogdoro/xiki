class CoffeeScript

  # Called by keyboard shortcut
  def self.run_block
    # Get block contents
    txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

    result = self.to_js txt
    # Insert result at end of block
    orig = Location.new
    View.cursor = right
    Line.to_left
    View.insert ">>\n#{result}\n\n"
    orig.go
  end

  def self.to_js txt

    path = "/tmp/tmp.coffee"

    # If txt is file path, us it
    if txt !~ /\n/ && txt =~ /\.coffee$/
      path = txt
    else
      File.open(path, "w") { |f| f << txt }   # Write to temp file
    end

    # Call js
    result = Console.run "coffee -pc \"#{path}\"", :sync=>true
    result.split("\n")[1..-2].join("\n")
  end

end

Keys.do_as_coffee { CoffeeScript.run_block }
