class Javascript
  def self.run

    Block.do_as_something do |txt|

      txt << "
        function p(txt) {
          print(txt);
        }";

      result = self.run_internal txt
    end
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
    Tree.under result, :escape=>''
  end

  def self.enter_as_jquery
    clip = Clipboard.get(0)
    insert_this = clip =~ /^[a-z_.#]+$/ ? clip : ""

    if Keys.prefix_u?
      View.insert "$(\"#{insert_this}\")"
      Search.backward '"'
      return
    end

    View.insert "- js/$(\"#{insert_this}\").blink()"
    Search.backward '"'

  end

end
