module Xiki
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

    def self.run
      Block.do_as_something do |txt|
        result = self.run_internal txt
      end
    end

    def self.run_internal txt
      # Write to temp file
      File.open("/tmp/tmp.py", "w") { |f| f << txt }
      # Call js
      Console.run "python /tmp/tmp.py", :sync=>true
    end
  end
end
