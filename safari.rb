class Safari
  def self.reload

    $el.do_applescript %Q<
      tell application "Safari"
        do JavaScript "window.location.reload();" in the first document
      end tell
      >

  end
end
