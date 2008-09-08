require 'net/telnet'

class Firefox
  def self.menu
    puts "
      .reload
      .eval \"alert('hey')\"
      .eval \"create()\"
      "
  end
  def self.run txt
    firefox = Net::Telnet::new("Host" => "localhost", "Port" => 9997)
    firefox.cmd "var w0 = getWindows()[0]"
    firefox.cmd "var browser = w0.getBrowser()"
    firefox.cmd "var window = browser.contentWindow"
    firefox.cmd "var document = browser.contentDocument"
    firefox.cmd txt
  end
  def self.reload
    self.run "browser.reload()"
  end
  def self.eval txt
    self.run "window.#{txt}"
  end

end
