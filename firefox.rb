require 'net/telnet'
require 'socket'

=begin
  Usage:
    p Firefox.value "window.title"
      "The Title"
    Firefox.run "alert('hey')"
=end

class Firefox
  def self.menu
    puts "
      .reload
      .exec \"alert('hey')\"
      .exec \"create()\"
      "
  end

  def self.reload
    Code.open_log_view if Keys.prefix_u && View.buffer_visible?('*output - tail of #{Ol.file_path}')
    prefix = Keys.prefix_n
    if prefix   # If numeric prefix, go to that tab
      tab = prefix - 1
      if tab == -1   # If 0, close tab
        Firefox.exec "getWindows()[0].getBrowser().removeCurrentTab();"
      else
        self.exec "getWindows()[0].getBrowser().tabContainer.selectedIndex = #{tab};"
      end
    else
      self.exec "getWindows()[0].getBrowser().reload()"
    end

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

  def self.connection

    socket = TCPSocket::new("localhost", "9997")
    socket.sync = true
    read_socket(socket)

    vars = "var window = getWindows()[0];"
    vars += "var browser = window.getBrowser();"
    vars += "var document = browser.contentDocument;"
    vars += "var body = document.body;"

    socket.send("#{vars}\n", 0)
    read_socket(socket)

    socket
  end

  def self.run txt
    socket = self.connection
    txt.gsub!("\n", ' ')
    txt.gsub!('"', "\\\"")

    socket.send "document.location = \"javascript: #{txt}; void(0)\"\n", 0
    read_socket(socket)
    nil

    # TODO 1 try this!
    #     socket.close
  end

  def self.value txt
    socket = self.connection
    txt.gsub!("\n", ' ')
    txt.gsub!('"', "\\\"")

    socket.send "#{txt};\n", 0
    read_socket(socket)

    # TODO 1 try this!
    #     socket.close
  end

  def self.url txt
    self.run "window.location = '#{txt}'"
  end

end
