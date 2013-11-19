module Xiki
  class Javascript

    def self.menu
      "
      | See
      @js/
      "
    end

    def self.run

      Block.do_as_something do |txt|

        txt << "
          function p(txt) {
            var json = JSON.stringify(txt);
            if(json == undefined && txt) json = txt;   // Sometimes JSON.stringify returns undefined when something there (like functions)
            print(json);
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
      Tree.under result, :escape=>'', :no_slash=>1
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

    def self.wrap_jquery_load txt, url=nil

      url ||= "http://code.jquery.com/jquery.min.js"
      # Temp hard-code to local!
      #       url ||= "http://xiki.loc/ajax/libs/jquery/1.9.1/jquery.min.js"

      txt = "
        var f = function(){
        #{txt}
        };

        if(typeof($) == 'undefined') {
          var s = document.createElement('script');
          s.src = '#{url}';
          s.onload = f;
          document.getElementsByTagName('head')[0].appendChild(s);
        }else{
          f();
        }
        ".unindent

      txt
    end

  end
end
