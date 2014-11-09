module Xiki
  class Serve
    def self.menu *args

      children = Tree.children(:string=>1)
      children = children.unindent if children

      trunk = Tree.path

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
        file = File.expand_path "~/xiki/commands/#{trunk[0]}.menu"
      else
        file = "/tmp/tmp.menu"
        File.open(file, "w") { |f| f << children }
      end

      code = self.wrap_controller file
      Node.run_controller code

      "<! showing in browser!"
    end

    def self.wrap_controller file
      %`
      var http = require('http');
      var Xiki = require('#{Xiki.dir}misc/js/xiki.js');
      var fs = require('fs');
      http.createServer(function(req, res) {

        url = decodeURI(req.url);
        console.log('processing request: ' + url);


        // If requesting js, just return it
        if(url.match(/^\\/js\\//)){
          res.writeHead(200, {'Content-Type': 'application/x-javascript'});
          var js = fs.readFileSync('#{Xiki.dir}misc/js/'+url.replace(/.+\\//, ''), 'utf8');
          res.end(js);
          return;
        }

        tree = fs.readFileSync(#{file.inspect}, 'utf8');

        if(req.headers['user-agent'] && ! req.headers['x-requested-with']){

          res.writeHead(200, {'Content-Type': 'text/html'});
          res.end(Xiki.render_mobile_ajax());

        }else{
          res.writeHead(200, {'Content-Type': 'text/plain'});
          console.log("tree:", tree);
          items = Xiki.children(tree, url);
          console.log("items:", items);
          res.end(items);
        }


      }).listen(8163, '127.0.0.1');
      console.log('Server running at http://127.0.0.1:8163/');
      `.unindent
    end

  end
end
