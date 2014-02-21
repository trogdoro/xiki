module Xiki
  class Mongo2
    MENU = %`
      - .collections/
      - docs/
        > Create
        @foo.save({_id:"a", txt:"b"})

        > Show
        @foo.find()
        @foo.find({_id:"a"})

        > Update
        @foo.update({_id:"a"}, {txt:"bbb"})

        > Delete
        @foo.remove({_id:"a"})
      @conf/
      `

    def self.collections db=nil, collection=nil, doc=nil
      conf = yield[:conf]

      # cut off all non-host stuff from the configuration
      dbhost = conf.sub(/\A> .+\n/, '').gsub(/\n/, "")

      collection.sub!(/\/$/, '') if collection

      # /, so list databases

      if ! db
        json = self.run('printjson(db._adminCommand("listDatabases"))', dbhost)
        o = JSON[json]
        return o["databases"].map{|d| "#{d['name']}/"}
      end

      # /db/, so list collections
      if ! collection
        json = self.run('printjson(db.getCollectionNames())', dbhost, db)
        o = JSON[json]
        return o.map{|d| "#{d}/"}
      end

      # /db/, so list records
      if ! doc
        txt = self.run("db.#{collection}.find()", dbhost, db)
        txt = %Q`> No documents yet.  Create one?\n{ "_id":"id", "name":"Steve", "description":"whatever"}` if txt == ""
        return txt.gsub(/^/, '| ')
      end

      # /db/doc, so save

      doc.sub!(/^\| /, '')
      command = "db.#{collection}.save(#{doc})"

      txt = self.run(command, dbhost, db)   # .gsub(/^/, '| ')

      "@flash/- saved!"

    end

    def self.run command, dbhost, db=nil

      command << ".forEach(printjson)" if command =~ /.find\(/
      cmd = "mongo #{dbhost} --eval '#{command}'"

#      raise cmd
      if db == nil
        txt = `mongo #{dbhost} --eval '#{command}'`
      else
        txt = `mongo #{dbhost}/#{db} --eval '#{command}'`
      end

      if txt =~ /couldn't connect to server/
        raise "> Couldn't connect - maybe it's not configured?
               @mongo2/@conf/
          ".unindent
      end

      txt.sub /(.+\n){2}/, ''   # Delete first 2 lines
    end

    def self.init

      # - Is this being called anywhere?
      #   - Maybe have item in menu that saves it to startup
      #     - maybe just shows file syntax for adding to a file!

      Launcher.add(/^db\./) do |l|   # General db... lines
        l.strip!
        txt = self.run l
        Tree.under "#{txt.strip}\n", :escape=>'| ', :no_slash=>1
      end

      Launcher.add(/^(\w+)\.(save|update)\(/) do |l|   # Shortcut for foo.save()
        l.strip!
        l = "db.#{l}"
        txt = self.run l
        Tree.under "done#{txt.strip}\n", :escape=>'| ', :no_slash=>1
      end

      Launcher.add(/^(\w+)\.find\(/) do |l|   # Shortcut for foo.find()
        l.strip!
        l = "db.#{l}"
        txt = self.run l
        Tree.under "#{txt.strip}\n", :escape=>'| ', :no_slash=>1
      end

      Launcher.add(/^(\w+)\.remove\(/) do |l|   # Shortcut for foo.remove()
        l.strip!
        l = "db.#{l}"
        txt = self.run l
        Tree.under "#{txt.strip}\n", :escape=>'| ', :no_slash=>1
      end

    end
  end
  Mongo2.init
end
