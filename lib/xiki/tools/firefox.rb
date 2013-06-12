require 'net/telnet'
require 'socket'

gem 'simple-tidy'
require 'simple-tidy'
# gem 'nokogiri-pretty'
# require 'nokogiri-pretty'

require "rexml/document"

module Xiki
  class Firefox

    @@log_unique_token = "aa"
    @@jquery_url = "http://code.jquery.com/jquery.min.js"
    #   @@jquery_url = "http://xiki.loc/assets/js/jquery.js"


    def self.menu
      %`
      @html/
      @js/
      @dom/
      - .navigate/
        - .reload/
        - .back/
        - .forward/
      - .tabs/
      - load js into a page) .include_jquery_and_utils
      - see/
        - @append/
        - @click/
        - @blink/
        - @ffo/
        - @jso/
        - @jsp/
        - @jsc/
      - api/
        | p Firefox.value "window.title"
        |   "The Title"
        | Firefox.exec "alert('hey')"
      - docs/
        > Js examples
        @js/p('hi')
        @js/$('a').blink()
        @js/p($('a').length)
        @js/$(':text').toggle()
        |
        > Running html in the browser
        @html/<b>hello</b>
        |
        |
        > Running javascript in the browser
        | Run some javascript:
        @js/alert('hi')
        |
        | Run and insert output:
        @jso/document.title
        |
        | Run and print output in floating divs:
        @jsp/document.title
        |
        | Run and print output in console:
        @jsc/document.title
        |
        |
        > Exploring and maniplating the current page
        | Drill into the dom:
        @dom/
        |
        | Search in the current page for "and":
        @ff/and/
        |
        | Make all a tags blink:
        @blink/a
        |
        | Simulate clicking the "sign up" link:
        @click/
        |
        | Drill into the properties of the window class:
        @ffo/window/
      `
    end

    def self.js txt=nil
      return Tree.<<("| $('h1').toggle(1000)  // Type some javascript here (to run in the browser)") if ! txt

      Firefox.exec txt
    end

    def self.coffee txt=nil
      return View.prompt("Type some coffeescript to run in the browser") if txt.nil?

      txt = CoffeeScript.run_internal txt

      Firefox.exec txt
      ".flash - ran in browser!"
    end

    def self.last_stack_trace
      Firefox.value('window.content.tmp_stack')
    end

    def self.reload

      # Clears out OL log
      Code.open_log_view if Keys.prefix_u && View.buffer_visible?('*ol')

      prefix = Keys.prefix_n :clear=>true
      if ! prefix
        Firefox.exec("gBrowser.reload()", :browser=>true)
      elsif prefix == 0
        # Similar to reload, but field values will reset
        Firefox.exec("document.location = document.location;")
      else

        tab = prefix - 1
        if tab == -1   # If 0, close tab
          self.close_tab
        else   # If number, switch to tab
          Firefox.exec("gBrowser.tabContainer.selectedIndex = #{tab}", :browser=>true)
        end

      end
      nil
    end

    def self.close_tab
      times = Keys.prefix_n :clear=>true
      (times||1).times do
        self.exec "gBrowser.removeCurrentTab();", :browser=>true
      end
    end

    def self.click
      link = Keys.input(:prompt=>'Substring of link to click on: ')

      Firefox.exec("
        var a = $('a:contains(#{link}):first');
        var url = a.attr('href');
        if(url == '#')
          a.click();
        else
          window.location = url;
      ")
    end

    def self.back
      Firefox.exec "history.back()"
    end

    def self.forward
      Firefox.exec "history.forward()"
    end

    #     # Copied from here (and modified):
    #     - /docs/tools/firewatir/firewatir-read-only/FireWatir/
    #       - container.rb
    #         |     def read_socket(socket = jssh_socket)
    def self.read_socket(socket)
      result = ""
      data = ""
      receive = true
      s = nil
      while(s == nil) do
        s = Kernel.select([socket] , nil , nil, 1)
      end
      for stream in s[0]
        data = stream.recv(1024)
        while(receive)
          result += data
          if(result.include?("\n> "))
            receive = false
          else
            data = stream.recv(1024)
          end
        end
      end

      length = result.length

      if length <= 3
        result = ""
      elsif(result[0..2] == "\n> ")
        result = result[3..length-1]
      else
        result = result[0..length-4]
      end
      if(result[result.length - 3..result.length - 1] == "\n> ")
        result = result[0..result.length - 4]
      end
      if(result[0..2] == "\n> ")
        result = result[3..result.length - 1]
      end
      result
    end




    def self.exec_block

      prefix = Keys.prefix

      # Get block contents
      txt, left, right = View.txt_per_prefix prefix

      funcs = %q`
        function p(s) {
          if(s == null)
            s = "[blank]";

          try {prepend_index++;}
          catch(e) { prepend_index = 0; }

          var d = document.createElement('div');
          document.body.appendChild(d);
          d.innerHTML = '<div style="top:'+(prepend_index*13)+'px; margin-left:5px; position:absolute; font-size:10px; z-index:1002; color:#000; filter: alpha(opacity=85); -moz-opacity: .85; opacity: .85; background-color:#999;">'+s+'</div>';

        }
      `

      # If started with <..., treat it as html

      return Browser.html txt if txt =~ /\A\s*</

      Browser.js txt
      return
    end

    def self.exec txt, options={}

      result = Firefox.mozrepl_command txt, options

      if result =~ /\$ is not defined/   # If no jquery wrap it and try again
        txt = Javascript.wrap_jquery_load txt
        result = Firefox.mozrepl_command txt, options
      elsif result =~ /\bp is not defined\b/   # If no jquery wrap it and try again
        txt = Javascript.wrap_jquery_load txt, "http://xiki.org/javascripts/util.js"
        result = Firefox.mozrepl_command txt, options
      end

      result.sub! /^"(.+)"$/m, "\\1"   # Remove quotes

      result

    rescue Errno::ECONNREFUSED

      # If brawser wasn't running, ask if they want to open it

      raise "> Looks like Firefox isn\'t open.  Open it?\n@app/Firefox/\n\nOr, maybe the MozRepl Firefox extension isn't installed and on."

    end

    def self.value txt
      self.exec(txt).sub(/\A"/, "").sub(/"\z/, "")
    end

    def self.url url=nil, options={}

      return ".prompt Type a url here." if url.empty?

      return $el.browse_url(url) if options[:new]

      reload = "gBrowser.reload();";
      reload = "" if options[:no_reload];

      js = %`
        var browsers = gBrowser.browsers;

        if((browsers[gBrowser.tabContainer.selectedIndex]).contentDocument.location.href == "#{url}"){
          #{reload}
          false;
        }else{
          var found = false;
          for(var i=0; i < browsers.length; i++) {
            if(browsers[i].contentDocument.location.href != "#{url}") continue;
            gBrowser.tabContainer.selectedIndex = i;
            #{options[:reload] ? reload : ''}
            found = true;
            break;
          }
          if(! found) (browsers[gBrowser.tabContainer.selectedIndex]).contentDocument.location.href = "#{url}";
        }

        `.unindent

      result = self.exec js, :browser=>true

      $el.browse_url(url) if result =~ /^!!! TypeError: gBrowser is null/

      nil
    end


    def self.do_as_html
      # Grab block
      txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

      self.html txt   # Write to temp file
    end

    def self.html txt=nil

      File.open("/tmp/tmp.html", "w") { |f| f << txt }

      Firefox.url "file:///tmp/tmp.html", :reload=>1

      nil
    end

    def self.append html=nil
      if html.nil?
        View.prompt("Add something to append")
        return "| "
      end

      html.gsub! '"', '\\"'
      html.gsub! "\n", ' '

      code = "$('body').append(\"#{html}\")"
      result = Firefox.exec code
      nil
    end

    def self.jso txt=nil
      return View.prompt("Add some js to output its result") if txt.nil?
      Firefox.exec txt
    end

    def self.enter_log_javascript_line

      $el.open_line(1) unless Line.blank?

      prefix = Keys.prefix

      if prefix.nil?
        View.insert "p('js#{@@log_unique_token}');"
        @@log_unique_token.next!

      elsif prefix == :u
        View.insert "pp('js#{@@log_unique_token}');"
        @@log_unique_token.next!

      elsif prefix == :-
        View.insert "p_stack();"

      elsif Keys.prefix_uu

        txt = Firefox.value('window.content.tmp_stack')
        matches = txt.scan(/\$pu.+?:\d+/)

        txts = matches.map{|o| "- #{o.gsub(/.+\//, '')}"}
        paths = matches.map{|o| Bookmarks[o]}

      end

      Line.to_left
    end

    def self.include_jquery_and_utils

      Firefox.exec "
        var s=document.createElement('script');
        s.setAttribute('src', '#{@@jquery_url}');
        document.getElementsByTagName('head')[0].appendChild(s);

        var s=document.createElement('script');
        s.setAttribute('src', 'http://xiki.org/javascripts/util.js');
        document.getElementsByTagName('head')[0].appendChild(s);
        ".unindent

      nil
    end

    def self.enter_as_url
      if Keys.prefix_u
        self.exec "gBrowser.tabContainer.selectedIndex += 1", :browser=>true
      end

      url = Firefox.value('document.location.toString()')
      View.insert url.gsub '%20', '+'
      View.insert("\n") if Keys.prefix_u
    end

    def self.mozrepl_read s
      begin
        timeout(6) do
          r = ''
          loop do
            r << s.readchar.chr
            break if r =~ /^repl\d*> $/
          end
          r.sub /^repl\d*> /, ''
        end
      rescue Timeout::Error=>e
        raise "Seems like mozrepl isnt responding.  Is the internet down??"
      end

    end

    def self.mozrepl_command js, options={}
      s = TCPSocket::new("localhost", "4242")

      initial_crap = mozrepl_read s
      repl = initial_crap[/repl\d+/] || 'repl'
      if options[:tab]   # Run js in page on nth tab
        s.puts("#{repl}.enter(window.gBrowser.getBrowserAtIndex(#{options[:tab]}).contentDocument)\n;")
        mozrepl_read s
      elsif options[:browser]   # Run js at browser outer level
        # Do nothing to change context
      else   # Run js in page
        s.puts("#{repl}.enter(content.wrappedJSObject)\n;")
        mozrepl_read s
      end

      s.puts js
      txt = mozrepl_read s
      s.close
      txt.strip
    end


    # TODO Not used any more?
    def self.log
      View.open "/Users/craig/.emacs.d/url_log.notes"
    end

    def self.xul txt
      Firefox.mozrepl_command txt, :browser=>true
    end


    def self.do_as_xul
      Block.do_as_something do |txt|
        Firefox.mozrepl_command txt, :browser=>true
      end
    end

    def self.tabs *urls
      if urls.any?
        url = urls[-1]   # If nested quotes, just use the last
        url.sub! /^\| /, ''
        self.url url, :new=>Keys.prefix_u
        return
      end

      js = '
        var browsers = gBrowser.browsers;
        txt = "";
        for(var i = 0; i < browsers.length; i++)
          txt += "| "+browsers[i].contentDocument.location.href+"\n";
        txt
        '.unindent

      result = Firefox.mozrepl_command js, :browser=>true
      result.gsub! /\/$/, ''
      result.sub(/\A"/, '').sub(/"\z/, '')
    end

    def self.load_jquery_maybe txt=nil #, options={}

      # If text passed, check it and error if complaining about jquery missing

      if txt && txt !~ /( (\$|p) is not defined|blink is not a function)/
        return nil   # Text without error, so don't load
      end

      self.exec "
        if(! document.getElementById('jqid')){
          var s=document.createElement('script');
          s.setAttribute('src', '#{@@jquery_url}'); s.setAttribute('id', 'jqid');
          document.getElementsByTagName('head')[0].appendChild(s);

          var s=document.createElement('script');
          s.setAttribute('src', 'http://xiki.org/javascripts/util.js');
          document.getElementsByTagName('head')[0].appendChild(s);
        }
        ".unindent

      raise ".flash - Added required js libs into page, try again!"
    end

    def self.object name=nil, key=nil

      if name.nil? || name.empty?
        return View.prompt "Type the name of an object in firefox"
      end

      if key.nil?

        result = Firefox.value %`
          var result = "";
          for(var key in #{name}) {
            if(! #{name}.hasOwnProperty(key)) continue;
            result += key+"\\n";
          }
          `.unindent
        return result.split("\n").sort{|a, b| a.sub(/^_+/, '').downcase <=> b.sub(/^_+/, '').downcase}.join("\n").gsub /.+/, "- \\0/"
      end

      # Key was passed

      result = Firefox.value "#{name}['#{key}']"

    end

    def self.blink txt
      return View.prompt("Type a selector to blink in firefox", :times=>5) if txt.nil?
      selector = Tree.slashless txt

      code = "
        jQuery.fn.blink = function(times, orig) {
          for(x=1;x<=2;x++) { jQuery(this).animate({opacity: 0.0}, {easing: 'swing', duration: 200}).animate({opacity: 1.0}, {easing: 'swing', duration: 200}) }
          return this;
        }
        jQuery(\"#{selector}\").blink();
        "

      txt = Firefox.exec code
      nil
    end

  end
end
