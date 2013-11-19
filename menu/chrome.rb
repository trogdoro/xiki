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

    Tree.unquote! txt
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
    return "> Type some js to run in chrome\n: alert('hi')\n" if args.blank?

    txt = args[0]

    # If html...

    if txt =~ /\A</
      self.html txt
      return "@flash/- Opened in chrome!"
    end

    # If js...
    self.js txt
    nil
  end
end
