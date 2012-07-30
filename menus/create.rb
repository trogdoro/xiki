class Create
  def self.menu name

    txt = File.read "/tmp/post_tmp"

    extension = txt =~ /^class / ? "rb" : "menu"
    file_path = File.expand_path("~/menus/#{name}.#{extension}")
    File.open(file_path, "w") { |f| f << txt }

    require_menu file_path

    "
    > Your menu was saved! Now go to it:
    - <a href='/#{name}'>#{name}</a>
    "
  end
end
