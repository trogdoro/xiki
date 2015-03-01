module Xiki
  class Rethink

    MENU_HIDDEN = "
      - .start/
      "

    MENU = "
      - .options/
        - .dbs/
        - .start/
      - notes/
      =conf/
      "
      # - examples/
    #       =notes/

    def self.dbs # name=nil
      options = yield
      @@conf = Xik.new options[:conf]

      # /, so list databases
      txt = r.db_list().run.map{|o| "<<< ~#{o}/\n"}.join
      "#{txt}| Or, set the default db using =conf"
    end

    def self.start

      local = "/usr/local/var/rethinkdb/"
      dir = File.directory?(local) ? local : "/tmp/"

      Shell.async "rethinkdb", :buffer=>"rethinkdb", :dir=>dir
      ""
    end

    def self.r database=nil
      require 'rethinkdb'
      @@r ||= RethinkDB::RQL.new

      self.conn(database).repl   # Means it'll allow .run without an arg

      @@r
    end

    def self.conn database=nil

      # Only do if there is a conf - fail silently if no conf!"]

      connect_options = {host:"localhost", port:"28015", db:"test"}

      ["host", "port", "db", "auth_key"].each do |key|
        val = @@conf[key]
        if val
          connect_options[key.to_sym] = val.sub(/\n.+/m, '')
        end
      end

      connect_options[:db] = database if database

      # Should this be ||= ?  Will it make it faster?
      @@conn = @@r.connect connect_options

      #     rescue Exception=>e
      #       raise "> Server isn't running\n+ start/"
    end

    def self.run_code txt, options={}

      @@conf = Xik.new options[:conf]

      txt, out, exception = Code.eval txt, :pretty_exception=>1, :target_module=>self

      return exception if exception

      Tree.quote out || txt

    end

    def self.menu_after output=nil, *args

      database = nil

      # ~foo, so pull it off as database
      if args[0] =~ /^~(.+)/
        database = $1
        args.slice! 0
      end

      table, key, content = args

      options = yield
      @@conf = Xik.new options[:conf]

      # /, so prepend all tables...

      if args == []
        begin
          tables = r(database).table_list().run.map{|o| "+ #{o}/\n"}.join

          if tables.blank?
            return "
              > No tables exist.  Create one?
              | Rename the next line and expand it:
              + new_table_name/
              ".unindent+output
          end

          return "#{tables}#{output}"
        rescue Exception=>e

          return "> Database doesn't exist\n- create it/" if e.message =~ /\ADatabase `.+` does not exist.$/

          raise "> Server isn't running.  Start it?\n#{output.sub('options', 'start')}" if e.is_a?(Errno::ECONNREFUSED)
          raise e
        end
      end

      # MENU handled it (output) so do nothing...

      return if output

      # 1st arg is code (multiline), so run it...

      if table =~ /\n/
        txt = self.run_code table, options
        return txt
      end

      # /table, so show all keys...

      if table && ! key
        # If "create it", create table
        if table == "create it"
          r.db_create(database || @@conf["db"]).run
          return "<! created mate!"
        end


        begin
          result = r(database).table(table).order_by("id").run.map{|o| "+ #{o["id"]}/\n"}.join

          # If no results, use sample id
          result = "> Table is empty.  Create some docs?\n+ foo/\n+ bar/" if result.blank?

          return result
        rescue Exception=>e
          # If table didn't exist, prompt them to create
          return "> Table doesn't exist\n- create it/" if e.message =~ /\ATable `.+` does not exist.$/
          raise e
        end
      end

      # /table/key, so show record...

      if key && ! content

        if task = options[:task]
          return "
            ~ delete
            ~ duplicate
            " if task == []
        end

        # If "create it", create table
        if key == "create it"
          r(database).table_create(table).run
          return "<! created!"
        end

        if task == ["delete"] || options[:prefix] == "delete"   # If delete, kill it
          r(database).table(table).get(key).delete.run
          View.flash "- deleted!", :times=>1
          Line.delete
          return
        end

        doc = r(database).table(table).get(key).run

        # If there was no doc, create sample one
        doc ||= {"name"=>"not there", "description"=>"This doc doesn't exist yet!  Modify and expand to create"}

        doc.delete "id"
        return Tree.quote TextUtil.ap(doc), :char=>"|"
      end

      # /table/key/content, so save...

      doc = TextUtil[content]
      doc["id"] = key
      result = r(database).table(table).insert(doc, :upsert=>true).run

      "<! saved!"

    end

  end

end
