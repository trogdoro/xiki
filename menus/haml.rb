class Haml
  def self.menu *args
    txt = ENV['txt']
    File.open("/tmp/tmp.haml", "w") { |f| f << txt }
    `haml /tmp/tmp.haml /tmp/tmp.html`

    # Then load in browser (or reload)
    Firefox.value('document.location.toString()') == "file:///tmp/tmp.html" ?
      Firefox.reload :
      $el.browse_url("file:///tmp/tmp.html")

    ".flash - Loaded in browser!"
  end
end
