require 'net/telnet'
require 'socket'

gem 'simple-tidy'
require 'simple-tidy'
gem 'nokogiri-pretty'
require 'nokogiri-pretty'

require "rexml/document"

=begin
  Usage:
    p Firefox.value "window.title"
      "The Title"
    Firefox.run "alert('hey')"
=end

class Firefox

  @@log_unique_token = "aa"

  def self.menu
    "
    - js/
    - .dom/
    - .tabs/
    - .reload
    - load js into a page: .include_jquery_and_utils
    - (js): p('hi')
    - (js): $('a').blink()
    - (js): p($('a').length)
    - (js): $(':text').toggle()
    "
  end

  def self.last_stack_trace
    Firefox.value('window.content.tmp_stack')
  end

  def self.reload
    Code.open_log_view if Keys.prefix_u && View.buffer_visible?('*output - tail of #{Ol.file_path}')
    prefix = Keys.prefix_n :clear=>true
    if prefix   # If numeric prefix, go to that tab
      tab = prefix - 1
      if tab == -1   # If 0, close tab
        self.close_tab
      else
        Firefox.mozrepl_command("gBrowser.tabContainer.selectedIndex = #{tab}", :browser=>true)
      end
    else
      Firefox.mozrepl_command("gBrowser.reload()", :browser=>true)
    end
    nil
  end

  def self.close_tab
    times = Keys.prefix_n :clear=>true
    (times||1).times do
      self.run "gBrowser.removeCurrentTab();", :browser=>true
    end
  end

  # Called internally by others
  def self.exec txt
    begin
      con = Net::Telnet::new("Host" => "localhost", "Port" => 9997)
      con.cmd txt
    rescue
      View.message "JSSH appears to be down!"
      ""
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

    # Get block contents
    txt, left, right = View.txt_per_prefix Keys.prefix

    funcs = %q|
      function p(s) {
        if(s == null)
          s = "[blank]";

        try {prepend_index++;}
        catch(e) { prepend_index = 0; }

        var d = document.createElement('div');
        document.body.appendChild(d);
        d.innerHTML = '<div style="top:'+(prepend_index*13)+'px; margin-left:5px; position:absolute; font-size:10px; z-index:1002; color:#000; filter: alpha(opacity=85); -moz-opacity: .85; opacity: .85; background-color:#999;">'+s+'</div>';

      }
    |

    # Remove comments
    txt.gsub! %r'^ *// .+', ''
    txt.gsub! %r'  // .+', ''

    Firefox.run "#{funcs}\n#{txt.gsub('\\', '\\\\\\')}"
    return

  end

  def self.run txt, options={}
    result = Firefox.mozrepl_command txt, options
    result.sub /^"(.+)"$/, "\\1"
  end

  def self.value txt
    self.run(txt).sub(/^"(.+)"$/, "\\1")
  end

  def self.url url, options={}
    return $el.browse_url(url) if options[:new]

    # Try to find it in a tab


    js = %`
      var browsers = gBrowser.browsers;

      found = false;

      if((browsers[gBrowser.tabContainer.selectedIndex]).contentDocument.location.href == "#{url}")
        false;
      else{
        for(var i = 0; i < browsers.length; i++) {
          if(browsers[i].contentDocument.location.href != "#{url}") continue;
          found = true;
          gBrowser.tabContainer.selectedIndex = i;
        }
        found;
      }
      `.unindent

    result = self.run js, :browser=>true

    self.run "window.location = '#{url}'" if result != "true"
  end

  def self.do_as_html
    # Grab block
    txt, left, right = View.txt_per_prefix #:prefix=>Keys.prefix

    # Write to temp file
    File.open("/tmp/tmp.html", "w") { |f| f << txt }

    # Then load in browser (or reload)
    Firefox.value('document.location.toString()') == "file:///tmp/tmp.html" ?
      Firefox.reload :
      $el.browse_url("file:///tmp/tmp.html")

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
      s.setAttribute('src', 'http://jquery.com/src/jquery-latest.js');
      document.getElementsByTagName('body')[0].appendChild(s);

      var s=document.createElement('script');
      s.setAttribute('src', 'http://xiki.org/javascripts/util.js');
      document.getElementsByTagName('body')[0].appendChild(s);
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
      timeout(4) do
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
    else   # Run js in page
      s.puts("#{repl}.enter(content.wrappedJSObject)\n;")
      mozrepl_read s
    end

    s.puts js
    txt = mozrepl_read s
    s.close
    txt.strip
  end

  def self.log
    View.open "/Users/craig/.emacs.d/url_log.notes"
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

  def self.dom *args

    prefix = Keys.prefix

    raw_args = args.to_s.sub /\/$/, ''

    # If last arg has linebreakes, save
    save = args.pop if args.last =~ /\n/

    args.each do |o|
      o.sub! /\/$/, ''
      o.sub!(/:(\d+)$/) { ":eq(#{$1.to_i - 1})" }
      o.sub!(/$/, ':eq(0)') if o !~ /[:#]/
    end
    args = ['html'] + args unless args[0] =~ /[.#]/
    args = args.join ' > '

    if save
      save.gsub! "\n", "\\n"
      save.gsub! '"', '\\"'

      js = %`
        var e = $(\"#{args}\");
        if(e.length)
          e.html(\"#{save}\");
        `.unindent

      if args =~ /(.+) > (.+)/
        parent, child = $1, $2.sub!(/:.+/, '')
        Ol << "parent: #{parent.inspect}"
        Ol << "child: #{child.inspect}"
        js << %`
          else
            $(\"#{parent}\").append(\"<#{child}>#{save}</#{child}>\");
          `.unindent
      end

      Firefox.run js
      return
    end

    js = %`
      var kids = [];
      `.unindent

    if prefix == :-
      Firefox.run "$(\"#{args}\").blink()"
      return
    end
    if prefix != 8 && prefix != 9
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

    kids = Firefox.run js
    kids = kids.sub(/\A"/, '').sub(/"\z/, '') if kids =~ /\A"/
    if kids =~ /\Ahtml::/
      kids = kids.sub(/\Ahtml::/, '').strip
      return prefix == 8 ?
        kids.gsub(/^/, '| ') :
        self.tidy(kids, raw_args)
    end

    if kids =~ /(\$ is not defined|blink is not a function)/
      self.load_jquery
      return "- Jquery required js libs into page, try again!"
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

  def self.load_jquery
    self.run "
      if(! document.getElementById('jqid')){
        var s=document.createElement('script');
        s.setAttribute('src', 'http://jquery.com/src/jquery-latest.js'); s.setAttribute('id', 'jqid');
        document.getElementsByTagName('body')[0].appendChild(s);

        var s=document.createElement('script');
        s.setAttribute('src', 'http://xiki.org/javascripts/util.js');
        document.getElementsByTagName('body')[0].appendChild(s);
      }
      ".unindent
  end
end

Launcher.add(/^#.+/) do |line|
  Line.delete :br
  View.insert "- Firefox.dom \"#{line}\"/"
  Launcher.launch
end

Launcher.add "dom" do |line|
  Firefox.dom(*line.split('/')[1..-1])
end

Launcher.add "js" do |line|
  line.sub! /^js\/?/, ''
  Firefox.run(line)
  nil
end

