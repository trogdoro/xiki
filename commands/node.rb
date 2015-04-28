module Xiki
  class Node
    MENU = %`
      | txt = "Some node code"
      | console.log(txt)
      - .controller/
      - docs/
        - headers/
          | console.log(JSON.stringify(req.headers, null, 2));
        - env vars/
          | res.write(JSON.stringify(process.env, null, 2));
        - method/
          | console.log(req.method);
        - url/
          | console.log(req.url);
        - request params/
          | var url = require("url");
          | var _url = url.parse(req.url, true);
          | console.log(JSON.stringify(_url, null, 2));
        - read a file sync/
          | var fs = require('fs');
          | var txt = fs.readFileSync('./recipe.txt', 'utf8');
          | console.log(txt);
        - run in prod mode/
          % export NODE_ENV=production
      `

    def self.menu_after output, *args
      return output if output

      if args.empty?
        View.prompt("Enter some code to run in node.js")
        return "| "
      end

      return self.block if args == ['block']

      txt = Tree.leaf args[0]
      result = Tree.quote JavascriptHandler.eval(txt)

      result
    end

    def self.wrap_controller code

      %`
      var http = require('http');
      http.createServer(function (req, res) {
        res.writeHead(200, {"Content-Type": "text/plain"});

      #{code}
      }).listen(1338, '127.0.0.1');
      console.log('Server running at http://127.0.0.1:1338/');
      `#.unindent

    end

    def self.controller *args

      return "
        | // This is a sample controller.  Edit and expand to run it.
        | console.log('got one request');
        | res.end('Hello Node World!');
        " if args.empty?

      code = self.wrap_controller args[0]
      self.run_controller code
    end

    def self.run_controller code
      File.open("/tmp/controller.js", "w") { |f| f << code }

      Buffers.delete "node" if View.buffer_open? "node"

      Shell.run "node controller.js", :dir=>"/tmp/", :buffer=>"node", :dont_move=>1
      $el.sit_for 0.2
      Firefox.url "http://localhost:1338"
      "<! showing in browser!"
    end

    def self.block
      left = Line.right + 1
      ignore, ignore, right = View.block_positions "^>"

      txt = self.run View.txt(right, left)
      Block >> txt

      nil
    end

    def self.run txt
      file = "/tmp/nodejs.js"
      txt = "function puts (txt){ return console.log(txt) }\n\n#{txt}"
      File.open(file, "w") { |f| f << txt }
      Shell["node #{file}"]
    end

  end
end
