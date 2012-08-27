class Node
  def self.menu
    "
    > Pass javascript to eval in node
    | puts(1 + 2)
    - .controller/
    - docs/
      - menu/
        > To try out controller code
        | @node/controller/
        |   | function

      - node/
        > Snippets
        @technologies/node_js/
    "
  end

  def self.menu_after output, *args
    return output if output

    if args.empty?
      View.prompt("Enter some code to run in node.js")
      return "| "
    end

    return self.block if args == ['block']

    txt = Tree.leaf args[0]
    result = Tree.quote self.run(txt)

    result
  end

  def self.wrap_controller code

    "
    var http = require('http');
    http.createServer(function (req, res) {

    #{code}
    }).listen(8161, '127.0.0.1');
    console.log('Server running at http://127.0.0.1:8161/');
    ".unindent

  end

  def self.controller *args

    return "
      | // Sample controller code
      | console.log('got a request');
      | res.end('Hello World');
      " if args.empty?

    code = self.wrap_controller ENV['txt']
    self.run_controller code
  end

  def self.run_controller code
    File.open("/tmp/controller.js", "w") { |f| f << code }

    Buffers.delete "node" if View.buffer_open? "node"

    Console.run "node controller.js", :dir=>"/tmp/", :buffer=>"node"
    $el.sit_for 0.2
    Firefox.url "http://localhost:8161"
    ".flash - showing in browser!"
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
    Console["node #{file}"]
  end

end
