require 'net/telnet'

class Firefox
  def self.menu
    puts "
      .reload
      .exec \"alert('hey')\"
      .exec \"create()\"
      "
  end
  def self.run txt
    txt.gsub!("\n", ' ')
    txt.gsub!('"', "\\\"")
    self.exec "getWindows()[0].getBrowser().contentDocument.location = \"javascript: #{txt}; void(0)\""
  end

  def self.reload
    prefix = Keys.prefix_n
    if prefix   # If numeric prefix, go to that tab
      tab = prefix - 1
      if tab == -1   # If 0, close tab
        Firefox.exec "getWindows()[0].getBrowser().removeCurrentTab();"
      else
        self.exec "getWindows()[0].getBrowser().tabContainer.selectedIndex = #{tab};"
      end
    end

    self.exec "getWindows()[0].getBrowser().reload()"
  end

  # Called internally by others
  def self.exec txt
    con = Net::Telnet::new("Host" => "localhost", "Port" => 9997)
    con.cmd txt
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
end
