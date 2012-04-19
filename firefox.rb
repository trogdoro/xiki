require 'net/telnet'
require 'socket'

gem 'simple-tidy'
require 'simple-tidy'
gem 'nokogiri-pretty'
require 'nokogiri-pretty'

require "rexml/document"

class Firefox

  @@log_unique_token = "aa"

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
    - .search/
    - load js into a page) .include_jquery_and_utils
    - .roots/
      - @ff/
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
      | Firefox.run "alert('hey')"
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
    return "| $('div').toggle()  // Type some javascript here (to run in the browser)" if ! txt

    Firefox.run txt  #, :jquery=>1
  end

  def self.coffee txt=nil
    return View.prompt("Type some coffeescript to run in the browser") if txt.nil?

    txt = CoffeeScript.run_internal txt

    Firefox.run txt  #, :jquery=>1
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
      Firefox.run("gBrowser.reload()", :browser=>true)
    elsif prefix == 0
      # Similar to reload, but field values will reset
      Firefox.run("document.location = document.location;")
    else

      tab = prefix - 1
      if tab == -1   # If 0, close tab
        self.close_tab
      else   # If number, switch to tab
        Firefox.run("gBrowser.tabContainer.selectedIndex = #{tab}", :browser=>true)
      end

    end
    nil
  end

  def self.close_tab
    times = Keys.prefix_n :clear=>true
    (times||1).times do
      self.run "gBrowser.removeCurrentTab();", :browser=>true
    end
  end

  def self.click
    link = Keys.input(:prompt=>'Substring of link to click on: ')

    Firefox.run("
      var a = $('a:contains(#{link}):first');
      var url = a.attr('href');
      if(url == '#')
        a.click();
      else
        window.location = url;
    ")
  end

  def self.back
    Firefox.run "history.back()"
  end

  def self.forward
    Firefox.run "history.forward()"
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




  def self.run_block

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

  def self.run txt, options={}

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
  end

  def self.value txt
    self.run(txt).sub(/\A"/, "").sub(/"\z/, "")
  end

  def self.url url, options={}
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

    result = self.run js, :browser=>true

    nil
  end


  def self.do_as_html
    # Grab block
    txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

    self.html txt   # Write to temp file
  end

  def self.html txt=nil

    # When C-8, delegate to .dom to show whole body
    return Tree.<< Firefox.dom(:prefix=>"all") if Keys.prefix == "all"
    return Tree.<< Firefox.dom(:prefix=>"outline") if Keys.prefix == "outline"

    return "| <h1>Provide some html here.</h1>\n| <div>Then click to show in <span>the browser.</span></div>" if ! txt

    return "
      | Enter something here to show it in the browser.
      | Or, do enter+all to show the html of the current page.
      " if txt.nil?

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
    result = Firefox.run code  #, :jquery=>1
    nil
  end

  def self.jso txt=nil
    return View.prompt("Add some js to output its result") if txt.nil?
    Firefox.run txt  #, :jquery=>1
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

    Firefox.run "
      var s=document.createElement('script');
      s.setAttribute('src', 'http://code.jquery.com/jquery-latest.js');
      document.getElementsByTagName('head')[0].appendChild(s);

      var s=document.createElement('script');
      s.setAttribute('src', 'http://xiki.org/javascripts/util.js');
      document.getElementsByTagName('head')[0].appendChild(s);
      ".unindent

    nil
  end

  def self.enter_as_url
    if Keys.prefix_u
      self.run "gBrowser.tabContainer.selectedIndex += 1", :browser=>true
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

  def self.tabs url=nil
    if url
      url = Line.value.sub /^[ |]*/, ''
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

  def self.search txt=nil
    if txt.nil?
      return View.prompt "Enter a string to search for on the page"
    end

    js = %`
      var result="", depth=-1;
      $('*:contains(#{txt})').each(function(i, e){
        var tag = e.tagName.toLowerCase();
        e = $(e);
        var current_depth = e.parents().length;   // Only climb downward
        if(current_depth <= depth) return depth = 9999;   // Don't grab any more
        depth = current_depth;
        var prev_count = $(e).prevAll(tag).length;
        result += tag;
        if(prev_count > 0) result += ':'+(prev_count+1);
        result += '/';
      })
      result;
      `.unindent

    result = Firefox.run js  #, :jquery=>1

    result.sub "html/", "- @dom/"
  end

  def self.dom *args#, options={}

    options = args[-1].is_a?(Hash) ? args.pop : {}

    prefix = options[:prefix] || Keys.prefix

    raw_args = args.to_s.sub /\/$/, ''

    # If last arg has linebreakes, save
    save = args.pop if args.last =~ /^\|/

    args.each do |o|
      o.sub! /\/$/, ''
      o.sub!(/:(\d+)$/) { ":eq(#{$1.to_i - 1})" }
      o.sub!(/$/, ':eq(0)') if o !~ /[:#]/
    end
    args = ['html'] + args unless args[0] =~ /[.#]/
    args = args.join ' > '

    if save
      save = Tree.siblings(:all=>true).map{|i| "#{i.gsub(/^\| /, '')}\n"}.join('')
      save.gsub! "\n", "\\n"
      save.gsub! '"', '\\"'

      js = %`
        var e = $(\"#{args}\");
        if(e.length)
          e.html(\"#{save}\");
        `.unindent

      if args =~ /(.+) > (.+)/
        parent, child = $1, $2.sub!(/:.+/, '')
        js << %`
          else
            $(\"#{parent}\").append(\"<#{child}>#{save}</#{child}>\");
          `.unindent
      end

      Firefox.run js
      return
    end

    js = %`
      $.fn.blink = function() {
        var el = $(this);
        for(x=1;x<=2;x++) {
          el.animate({opacity: 0.0}, {easing: 'swing', duration: 200});
          el.animate({opacity: 1.0}, {easing: 'swing', duration: 200});
        }
        return this;
      };

      var kids = [];
      `.unindent

    if prefix == :-
      Firefox.run "$(\"#{args}\").blink()"
      return
    end
    if prefix != "all" && prefix != "outline"
      js << %`
        $("#{args}").blink().children().each(function(n, e){
          var tag = e.nodeName.toLowerCase();
          var id = $(e).attr('id');
          if(id) tag += "#"+id
          kids.push(tag);
        })
        `
    end

    js << %`
      if(kids.length)
        String(kids);
      else
        "html::"+$("#{args}").html();
      `.unindent

    kids = Firefox.run js  #, :jquery=>1

    kids = kids.sub(/\A"/, '').sub(/"\z/, '') if kids =~ /\A"/
    if kids =~ /\Ahtml::/
      kids = kids.sub(/\Ahtml::/, '').strip
      return prefix == "all" ?
        kids.gsub(/^/, '| ') :
        self.tidy(kids, raw_args)
    end

    kids = kids.split ','

    counts = {}
    kids.each do |k|   # Add on counts
      k_raw = k.sub /#.+/, ''
      counts[k] ||= 0
      counts[k] += 1
      k.replace "#{k}:#{counts[k]}" unless k =~ /#/ || counts[k] == 1
    end
    kids.map{|o| "#{o}/"}
  end

  def self.tidy html, tag=nil

    File.open("/tmp/tidy.html", "w") { |f| f << html }

    errors = `tidy --indent-spaces 2 --tidy-mark 0 --force-output 1 -i -wrap 0 -o /tmp/tidy.html.out /tmp/tidy.html`
    html = IO.read("/tmp/tidy.html.out")

    html.gsub! /\n\n+/, "\n"

    if tag == ""   # It's the whole thing, do nothing
    elsif tag == "head"
      html.sub! /.+?<head>\n/m, ''
      html.sub! /^<\/head>\n.+/m, ''
    else
      html.sub! /.+?<body>\n/m, ''
      html.sub! /^<\/body>\n.+/m, ''
    end

    html.gsub!(/ +$/, '')
    html.gsub! /^  /, '' unless html =~ /^</
    html.gsub! /^/, '| '
    html

      # Try Nokogiri and xsl - Fucking dies part-way through
      #       return Nokogiri::XML(kids).human.sub(/\A<\?.+\n\n/, '').gsub(/^/, '| ')
      #       return Nokogiri::XML("<foo>#{kids}</foo>").human.gsub(/^/, '| ')
      #       return Nokogiri::XML(kids).human.gsub(/^/, '| ')

      # Try REXML (gives errors)
      #       doc = REXML::Document.new("<foo>#{kids}</foo>")
      #       out = StringIO.new; doc.write( out, 2 )
      #       return out.string

  end

  def self.load_jquery_maybe txt=nil #, options={}

    # If text passed, check it and error if complaining about jquery missing

    if txt && txt !~ /( (\$|p) is not defined|blink is not a function)/
      return nil   # Text without error, so don't load
    end

    self.run "
      if(! document.getElementById('jqid')){
        var s=document.createElement('script');
        s.setAttribute('src', 'http://code.jquery.com/jquery-latest.js'); s.setAttribute('id', 'jqid');
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
      $.fn.blink = function(times, orig) {
        for(x=1;x<=2;x++) { $(this).animate({opacity: 0.0}, {easing: 'swing', duration: 200}).animate({opacity: 1.0}, {easing: 'swing', duration: 200}) }
        return this;
      }
      $(\"#{selector}\").blink();
      "

    txt = Firefox.run code  #, :jquery=>1
    nil
  end
end

Menu.ffo :class=>'Firefox.object'

Launcher.add(/^#.+/) do |line|
  Line.delete :br
  View.insert "- Firefox.dom \"#{line}\"/"
  Launcher.launch
end

Menu.dom do |line|
  Firefox.dom *Menu.split(line, :rootless=>1)
end

Menu.js do |path|
  Applescript.run("Firefox", "activate") if Keys.prefix_u
  Firefox.js Tree.rest(path)
  nil
end

Menu.coffee do |path|
  Firefox.coffee Tree.rest(path)
  nil
end

Menu.jsp do |path|
  txt = Tree.leaf path
  txt = txt.strip.sub(/;\z/, '')   # Remove any semicolon at end
  code = "p(#{txt})"
  result = Firefox.run code  #, :jquery=>1
  nil
end

Menu.jsc do |path|   # - (js): js to run in firefox
  txt = Tree.leaf path
  txt = txt.strip.sub(/;\z/, '')   # Remove any semicolon at end
  code = "console.log(#{txt})"
  Firefox.run code  #, :jquery=>1
  nil
end

Menu.blink do |path|
  Firefox.blink Tree.rest(path)
  nil
end


Menu.jso do |path|   # - (js): js to run in firefox
  txt = Firefox.jso Tree.rest(path)
  Tree.<< txt, :no_slash=>1
  nil
end

Menu.jsi do |path|   # - (js): js to run in firefox
  txt = Tree.rest(path)
  txt.sub! /;/, ''
  txt = "JSON.stringify(#{txt})"
  txt = Firefox.jso txt
  Tree.<< txt, :no_slash=>1
  nil
end

Menu.xul do |path|   # - (js): js to run in firefox
  Firefox.xul Tree.leaf(path)
end

Menu.ff do |line|
  Menu["firefox/search/#{Tree.rootless line}"]
end

Menu.click do |path|
  nth = 0
  txt = Tree.leaf path
  Firefox.run("$('a, *[onclick]').filter(':contains(#{txt}):eq(#{nth})').click()")  #, :jquery=>1)
  nil
end

Menu.append do |path|
  Firefox.append Tree.rest(path)
end
