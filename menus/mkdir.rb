class Mkdir

  def self.menu
    trunk = Xiki.trunk
    return "
      > Usage
      | Nest @mkdir under a dir path to create the dir, like this:
      |
      | - /tmp/newdir/
      |   - @mkdir
      |
      " if trunk.length <= 1

    return View.glow("- Dir already exists!") if File.exists?(trunk[-2])

    `mkdir -p "#{trunk[-2]}"`

    return View.glow "- Created!"
  end

end
