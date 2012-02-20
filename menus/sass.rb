class Sass
  def self.menu *args
    txt = ENV['txt']
    File.open("/tmp/tmp.sass", "w") { |f| f << txt }
    css = `sass /tmp/tmp.sass`

    css.gsub!("\n", '\n')
    css.gsub!('"', '\"')
    code = "$('head').append(\"<style>#{css}</style>\")"

    Firefox.run code

    ".flash - Loaded in browser!"
  end
end
