module Xiki
  class CoffeeScript

    # Called by keyboard shortcut
    def self.run_block
      Block.do_as_something do |txt|
        if Keys.prefix_u
          self.to_js(txt)
        else
          txt = "p = print = console.log\n\n#{txt}"
          self.execute(txt)
        end
      end
    end

    def self.execute txt
      Console.sync 'coffee -s', :stdin=>txt
    end

    def self.to_js txt
      Console.sync 'coffee -sc', :stdin=>txt
    end

  end

  Keys.do_as_coffee { CoffeeScript.run_block }
end
