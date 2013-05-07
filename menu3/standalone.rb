class Standalone
  def self.menu *args
    children = Tree.children(:string=>1)
    children = children.unindent if children

    trunk = Xiki.trunk

    # If no children and no parents, say how to use it.
    if children.blank? && trunk.length == 1
      return "
        > How to use
        | To make a stand-alone jquery mobile page and show in the browser,
        | put a menu under or above, like one of these:
        |
        | standalone/
        |   - dogs/
        |   - cats/
        |
        | animals/
        |   @standalone
        "
    end

    menu = "untitled"

    if children.blank?
      file = File.expand_path "~/menu/#{trunk[0]}.menu"
      menu = trunk[0]
      children = File.read file
    end

    # Write file to tmp
    File.open("/tmp/#{menu}.html", "w") { |f| f << self.wrap_standalone(children) }

    Firefox.url "file:///tmp/untitled.html"

    ".flash - showing in browser!"
  end

  def self.wrap_standalone children
    %`
    <head>
      <script src="http://code.jquery.com/jquery-1.6.4.min.js"></script>
      <script src="file://#{Xiki.dir}etc/js/xiki.js"></script>
      <script src="file://#{Xiki.dir}etc/js/menu1.js"></script>
      <script src="http://code.jquery.com/mobile/1.0/jquery.mobile-1.0.min.js"></script>
      <link rel="stylesheet" href="http://code.jquery.com/mobile/1.0/jquery.mobile-1.0.min.css" />
    </head>
    <body>
      <div data-xiki="menu">\n#{children}
      </div>
    </body>
    `.unindent

  end

end
