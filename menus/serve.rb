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


    if children.blank?
      file = File.expand_path "~/menus/#{trunk[0]}.menu"
    else
      file = "/tmp/tmp.menu"
      File.open(file, "w") { |f| f << children }
    end

    code = self.wrap_controller file
    Node.run_controller code

    ".flash - showing in browser!"
  end

  def self.wrap_controller file
    %`
    var http = require('http');
    var xiki = require('#{Xiki.dir}etc/js/xiki.js');
    var fs = require('fs');
    http.createServer(function (req, res) {

      console.log('processing request');
      tree = fs.readFileSync(#{file.inspect}, 'utf8');


      if(req.headers['user-agent'] && ! req.headers['x-requested-with']){

        res.writeHead(200, {'Content-Type': 'text/html'});
        res.end(xiki.render_mobile());

      }else{
        res.writeHead(200, {'Content-Type': 'text/plain'});
        items = xiki.children(tree, req.url);
        res.end(items);
      }


    }).listen(8161, '127.0.0.1');
    console.log('Server running at http://127.0.0.1:8161/');
    `.unindent
  end

end
