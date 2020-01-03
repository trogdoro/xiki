module Xiki
  class Node

    def self.run txt
      file = "/tmp/nodejs.js"
      txt = "function puts (txt){ return console.log(txt) }\n\n#{txt}"
      File.open(file, "w") { |f| f << txt }
      Shell["node #{file}"]
    end

  end
end
