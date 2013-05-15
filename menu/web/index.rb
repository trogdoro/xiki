require 'daemons'
require 'net/http'
class Web

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

    if status =~ /^error/i
      return "
        > Already running...
        #{Tree.quote status.strip}
        ".unindent +
        self.urls_message +
        "
        > Try stopping
        @web/control/stop/
        ".unindent
    end

    status = Tree.quote(status) if status.any?

    "> Started...\n#{self.urls_message}"
  end

  def self.urls_message
    "
    Go to this url in your web browser:
      http://localhost:8161
    Or, if you've setup apache per @web/setup/apache/, go to:
      http://xiki
    ".unindent
  end

  def self.stop
    output = Console.sync "ruby -rubygems sinatra_control.rb stop", :dir=>"#{Xiki.dir}etc/www/sinatra_server.rb"
    Tree.quote output
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
