require 'daemons'
require 'net/http'
class Web

  MENU_HIDDEN = "
    - .stop/
    "

  # What was this doing?
  def self.menu_after output, *path
    return if output

    if i = path.index("http:")
      url = path[i..-1].join('/')
      return Menu["http/#{url}"]
    end

    nil
  end

  def self.start *url

    return if url.any?   # .menu_after will take care of going to url

    status = Console.sync "ruby -rubygems sinatra_control.rb start", :dir=>"#{Xiki.dir}etc/www/sinatra_server.rb"

    # If not one of the 2 statuses we expect, error out...

    if status != "" and status !~ /^pid-file for killed process/
      return "
        > Error...
        #{Tree.quote status.strip}
        ".unindent +
        #self.urls_message +
        "
        > Try stopping if already running
        $ xiki web/control/stop/
        ".unindent
    end

    "> Started...\n#{self.urls_message}"
  end

  def self.urls_message
    "
    Go to this url in your web browser:
      http://localhost:8161
    Or, if you've set up apache per @web/setup/apache/, go to:
      http://xiki
    ".unindent
  end

  def self.stop
    output = Console.sync "ruby -rubygems sinatra_control.rb stop", :dir=>"#{Xiki.dir}etc/www/sinatra_server.rb"
    "> Stopping...\n#{Tree.quote output}"
  end
  def self.restart
    output = Console.sync "ruby -rubygems sinatra_control.rb restart", :dir=>"#{Xiki.dir}etc/www/"
    Tree.quote output
  end
  def self.status
    output = Console.sync "ruby -rubygems sinatra_control.rb status", :dir=>"#{Xiki.dir}etc/www/"
    Tree.quote output
  end

  # Reload the sinatra_server.rb code, and reloed the browser.
  def self.reload
    HTTParty.get("http://localhost:8161/_reload") rescue :exception
    Browser.reload
    "- reloaded!"
  end

end
