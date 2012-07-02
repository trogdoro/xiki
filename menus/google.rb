require "cgi"

class Google
  def self.menu *args

    if args.empty?   # If no path, pull from history
      return Launcher.last "google", :exclude_path=>1, :quoted=>1
    end
    txt = CGI.escape ENV['txt']
    #     txt = line.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
    $el.browse_url "http://www.google.com/search?q=#{txt}"
    nil

  end

  def self.insert
    line = Line.value
    return View << "google/" if line =~ /^$/
    return View << "@google/" if line =~ /^ /
    return View << "google/" if line =~ /^ *- $/

    View << "google/"
  end

  def self.search txt
    txt = CGI.escape txt
    Browser.url "http://www.google.com/search?q=#{txt}"
    nil
  end

  def self.maps txt
    txt = CGI.escape txt
    Browser.url "http://maps.google.com/maps?q=#{txt}"
    nil
  end

end
