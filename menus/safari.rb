class Safari
  def self.reload

    $el.do_applescript %`
      tell application "Safari"
        do JavaScript "window.location.reload();" in the first document
      end tell
      `

  end
end
