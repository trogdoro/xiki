#
# > .kill server
# ! result = SeleniumWrapper.quit
# ! result == "" ? "killed" : result
#
# > .check if server running
# ! RestTree.get("http://localhost:5313/ping", :raise=>1) rescue "not running"
#
# > .Run javascript in the browser
# ! SeleniumWrapper.js("return 1 + 2")
#
# > .Go to a url
# ! SeleniumWrapper.url("http://google.com")
#

if ENV['XIKI_DIR']
  require "#{ENV['XIKI_DIR']}/lib/xiki/core/ol.rb"
end

#
# How to run independently > for troubleshooting
#
# $ export GEM_HOME=~/.xiki/misc/gems/; ruby -e "load '/Users/craig/Dropbox/xiki/lib/xiki/tools/selenium_wrapper.rb'; Xiki::SeleniumWrapper.start_server"
#
module Xiki
  class SeleniumWrapper

    @@driver = {}

    @@jquery_url = "http://code.jquery.com/jquery.min.js"
    # @@jquery_url = "http://localhost:8162/js/jquery.min.js"

    #
    # > .select current tab
    # ! SeleniumWrapper.select_visible_tab
    #
    def self.select_visible_tab

      driver = self.driver

      # For each window
      driver.window_handles.each do |window|
        # Select to each window
        driver.switch_to.window window

        # Hidden, so ignore
        next if driver.execute_script("return document.hidden")

        # Visible, so stop looking and leave it selected
        return
      end

    end

    def self.driver
      begin
        require "selenium-webdriver"
      rescue LoadError
        puts 'Selenium gem not found. Install it with "xikigem install selenium"'
        # Todo > tell users they need to install selenium
        return nil   # Callers shauld do something else if nil
      end

      # kind = (Conf.get("driver", "driver") || "firefox").to_sym
      # For now > hard-code to chrome
      kind = "chrome".to_sym

      begin

        driver = @@driver[kind]
        if ! driver

          driver = @@driver[kind] = Selenium::WebDriver.for kind

          # Set to the right half of the window
          self.resize driver
        end

        # Hangs when no window now? > check number of windows
        # This seems to be necessary to make it not hang
        if driver.window_handles.length == 0
          @@driver[kind].quit   # Quit existing browser
          raise "no window"
        end

        # Do something trivial, so it'll error if not connected
        driver.execute_script("return 1")

      rescue Exception=>e

        driver = @@driver[kind] = Selenium::WebDriver.for kind

        self.resize driver

      end

      @@driver[kind]

    end

    def self.resize driver
      # driver.manage.window.move_to(720, 20)
      # driver.manage.window.resize_to(720, 865)
      driver.manage.window.move_to(750, 20)
      driver.manage.window.resize_to(670, 855)
    end

    def self.url txt
      # Make sure server is running
      self.ensure_server_running

      RestTree.post("http://localhost:5313/url", :raise=>1, :body=>txt)
    end

    def self.url_internal txt
      # Always go to the visible tab first
      self.select_visible_tab

      self.driver.navigate.to txt
    end


    def self.start_server

      require 'webrick'
      server = WEBrick::HTTPServer.new :Port=>5313
      trap('INT') { server.shutdown }

      server.mount_proc '/js' do |req, res|
        txt = self.js_internal req.body
        res.body = txt.to_s
      end
      server.mount_proc '/quit' do |req, res|
        server.shutdown
      end
      server.mount_proc '/url' do |req, res|
        txt = self.url_internal req.body
        res.body = txt.to_s
      end

      server.mount_proc '/ping' do |req, res|
        res.body = "pong"
      end

      server.start

    end

    def self.ensure_server_running
      # See if we get a response!
      txt = RestTree.get("http://localhost:5313/ping", :raise=>1)
    rescue Exception=>e

      # Not running, so start
      Shell.sync %`export GEM_HOME=~/.xiki/misc/gems/; ruby -e "load '#{XIKI_DIR}/lib/xiki/tools/selenium_wrapper.rb'; Xiki::SeleniumWrapper.start_server" &`

      # Give it time before we try to connect
      sleep 0.5
    end

    # Main entry point for running JavaScript. It'll spin off a server
    # in the background if One isn't running yet. It Delegates to
    # .js_internal.
    def self.js txt, options={}

      # Make sure server is running
      self.ensure_server_running

      RestTree.post("http://localhost:5313/js", :raise=>1, :body=>txt)
    end

    def self.quit
      RestTree.post "http://localhost:5313/quit" #, :raise=>1
    end

    def self.js_internal txt, options={}
      driver = self.driver

      raise "
        > A gem is required
        | The selenium-webdriver gem must be installed to send js
        | to the browser. To jump back to your shell and install
        | it as a xiki-local gem, type ^G on this line:
        |
        $ xikigem install selenium-webdriver
        " if ! driver


      # Always go to the visible tab first
      self.select_visible_tab


      txt = begin
        driver.execute_script txt
      rescue Exception=>e

        if e.message =~ /\Aunknown error: \$ is not defined\n/
          driver.execute_script "
            var s=document.createElement('script');
            s.setAttribute('src', '#{@@jquery_url}'); s.setAttribute('id', 'jqid');
            document.getElementsByTagName('head')[0].appendChild(s);

            var s=document.createElement('script');
            s.setAttribute('src', 'http://xiki.org/javascripts/util.js');
            document.getElementsByTagName('head')[0].appendChild(s);
            ".unindent

          wait = Selenium::WebDriver::Wait.new(:timeout => 10) # seconds
          wait.until { driver.execute_script("return typeof( $ )") != "undefined" }

          driver.execute_script txt
        end
      end

      # Selenium objects in result, so change to html

      if txt.class == Array
        return txt.map do |o|
          if o.class == Selenium::WebDriver::Element
            next o.attribute("outerHTML")
          end
          o
        end.join("\n")
      end

      return txt

    end

  end
end
