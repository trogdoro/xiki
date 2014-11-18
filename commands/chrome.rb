class Chrome

  def self.reload
    Xiki::Applescript.run %`
      tell application "Google Chrome"
        reload active tab of front window
      end tell
      `.unindent
  end

  def self.html txt, options={}

    file = options[:file] || "/tmp/chrome.html"

    File.open(file, "w") { |f| f << txt }

    Xiki::Applescript.run %`
      tell application "Google Chrome"
        set URL of active tab of front window to "file://#{file}"
      end tell
      `.unindent


    #     result = Xiki::Applescript.run txt

  end

  def self.js txt

    Xiki::Tree.unquote! txt
    txt.gsub! '"', '\"'
    txt.gsub! "\n", "\\n"

    txt = %`
        tell application "Google Chrome"
          tell window 1
            tell active tab
              execute javascript "#{txt}"
            end tell
          end tell
        end tell
        `

    result = Xiki::Applescript.run txt

    nil

  end

  def self.menu *args
Ol()
    return "> Type some js to run in chrome\n: alert('hi')\n" if args.blank?

    txt = args[0]

    # <html, so reder it...

    if txt =~ /\A</
      self.html txt
      return "<! Opened in chrome!"
    end

    # /url, so render url...

Ol "txt", txt
Ol()
    self.url(args[1..-1].join('/')) if txt == "url"

    # js, so eval it...

    self.js txt
    return


    nil
  end

  def self.url txt
Ol "txt", txt
    self.js %`window.location = "#{txt}"`
  end

end
