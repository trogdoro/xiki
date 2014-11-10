module Xiki
  class Safari
    def self.reload

      Applescript.run %`
        tell application "Safari"
          do JavaScript "window.location.reload();" in the first document
        end tell
        `

    end
  end
end
