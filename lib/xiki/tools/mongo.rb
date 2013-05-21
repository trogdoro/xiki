module Mongo
  def self.menu # bucket=nil
    %`
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
    `.unindent
  end

  def self.collections collection=nil

    collection.sub!(/\/$/, '') if collection

    # If /, list databases

    if collection.nil?
      json = self.run 'printjson(db._adminCommand("listDatabases"))'
      o = JSON[json]
      return o["databases"].map{|d| "#{d['name']}/"}
    end

    self.run("db.#{collection}.find()").gsub(/^/, '| ')
  end

  def self.run command
    command << ".forEach(printjson)" if command =~ /.find\(/
    txt = `mongo admin --eval '#{command}'`

    if txt =~ /couldn't connect to server/
      raise "> Mongo isn't running. Start it?
        @/tmp/
          % mongod
        ".unindent
    end

    txt.sub /(.+\n){2}/, ''   # Delete first 2 lines

  end

  def self.init

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

Mongo.init
