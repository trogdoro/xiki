module Xiki
  class Clojure

    MENU = %`
      : ;; Here's some clojure code, modify it and double-click!
      : (println "Hello")
      - .repl/
      - .remote/
        - .connect/
        - start/
          @ % ssh foo@bar.com
          | Start and connect to it
          @ % lein repl :start :port 1234
          | Start server and waits
          @ % lein repl :headless [:port port]
      - setup/
        - install clojure/
          > Install clojure via leiningen
          | How to install on the mac:
          @% brew install leiningen
      @conf/
      `

    def self.remote path=nil

      # /remote/, so list them...

      conf = yield[:conf]
      conf.sub(/\A> .+\n/, '').gsub(/.+/, "- \\0/")

      # /remote/foo.com, so connect or start...

      "TODO: not implemented yet.  Here's the path: #{path}"
    end

    def self.repl
      Console.run "lein repl"
      ""
    end

    def self.menu_before *args

      return if args.blank?   # Do nothing if just "clojure/"

      # If clojure jar not set, error out...

      conf = yield[:conf]
      return "
        > Expand this and add your clojure jar here first
        @clojure/@conf/
        |
        | Hint, it's probably somewhere under here:
        @~/.m2/repository/org/clojure/clojure/
        " if conf !~ /\.jar/
    end

    def self.menu_after output, *args
      return if args[0] !~ /\n/   # Only continue if :... lines passed in

      self.run args[0], yield   # Eval them
    end

    def self.run txt, options

      file = "/tmp/tmp.clj"
      File.open(file, "w") { |f| f << txt }

      conf = options[:conf]
      clojure_jar = conf[/.+\.jar$/]

      command = "java -cp #{clojure_jar} clojure.main -i #{file}"
      output = Console.sync command

      Tree.quote output
    end

  end
end
