require 'net/telnet'
require 'socket'

=begin
  Usage:
    p Firefox.value "window.title"
      "The Title"
    Firefox.run "alert('hey')"
=end

class Firefox

  @@log_unique_token = "aa"

  def self.menu
    ".reload
     .include_jquery_and_utils
     - (js): p('hi')
     - (js): $('a').blink()
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
        Firefox.mozrepl_command("window.getBrowser().tabContainer.selectedIndex = #{tab}", :browser=>true)
      end
    else
      Firefox.mozrepl_command("window.getBrowser().reload()", :browser=>true)
    end

  end

  def self.close_tab
    times = Keys.prefix_n :clear=>true
    (times||1).times do
      self.run "window.getBrowser().removeCurrentTab();", :browser=>true
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




  def self.run txt, options={}
    result = Firefox.mozrepl_command txt, options
    result.sub /^"(.+)"$/, "\\1"
  end

  def self.value txt
    self.run(txt).sub(/^"(.+)"$/, "\\1")
  end

  def self.url txt
    self.run "window.location = '#{txt}'"
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
"

  end

  def self.enter_as_url
    if Keys.prefix_u
      self.run "window.getBrowser().tabContainer.selectedIndex += 1", :browser=>true
    end

    View.insert Firefox.value('document.location.toString()');
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

end
