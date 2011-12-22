class Serve
  def self.menu *args

    children = Tree.children(:string=>1)
    children = children.unindent if children

    trunk = Xiki.trunk

    # If no children and no parents, say how to use it.
    if children.blank? && trunk.length == 1
      return "
        > How to use
        | To serve a menu in the browser, put a menu under or above, like one of these:
        |
        | serve/
        |   - dogs/
        |   - cats/
        |
        | animals/
        |   @serve
        "
    end

    File.open("/tmp/tmp.menu", "w") { |f| f << children }

    code = self.wrap_controller "/tmp/tmp.menu"
    Node.run_controller code

    ".flash - showing in browser!"
  end

  def self.wrap_controller file
    "
    var http = require('http');
    var xiki = require('#{Xiki.dir}etc/js/xiki.js');
    var fs = require('fs');
    http.createServer(function (req, res) {

    res.writeHead(200, {'Content-Type': 'text/plain'});

    tree = fs.readFileSync(#{file.inspect}, 'utf8');
    console.log(tree);

    items = xiki.children(tree, req.url);
    console.log(items);
    res.end(items);

    }).listen(8161, '127.0.0.1');
    console.log('Server running at http://127.0.0.1:8161/');
    ".unindent
  end

end
