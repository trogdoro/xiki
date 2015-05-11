module Xiki
  class Rethinkdb

    MENU_HIDDEN = "
      - .start/
      "

    MENU = "
      - .options/
        - .dbs/
        - .start/
      =conf/
    "

    def self.dbs # name=nil
      options = yield
      @@conf = Xik.new options[:conf]

      # /, so list databases...

      txt = r.db_list().run.map{|o| "<<< ~#{o}/\n"}.join
      "#{txt}| Or, set the default db using =conf"
    end

    def self.start

      local = "/usr/local/var/rethinkdb/"
      dir = File.directory?(local) ? local : "/tmp/"

      "<$$ cd #{dir}\nrethinkdb --daemon"
    end

    def self.r database=nil
      require 'rethinkdb'
      @@r ||= RethinkDB::RQL.new

      self.conn(database).repl   # Means it'll allow .run without an arg

      @@r
    end

    def self.conn database=nil

      # Only do if there is a conf - fail silently if no conf!"]

      connect_options = {
        host: @@conf["host"],
        port: @@conf["port"],
        db: @@conf["default database"],
      }

      if auth_key = @@conf["auth key"]
        connect_options[:auth_key] = auth_key
      end

      @@conn = @@r.connect connect_options
    end

    def self.run_code txt, options={}

      @@conf = Xik.new options[:conf]

      txt, out, exception = Code.eval txt, :pretty_exception=>1, :target_module=>self

      return exception if exception

      Tree.quote out || txt

    end

    def self.menu_after output=nil, *args

      options = yield

      # This is cached here, since options are passed in at this level
      @@conf = Xik.new options[:conf]

      database = @@conf['default database']

      # ~foo, so pull it off as database
      if args[0] =~ /^~(.+)/
        database = $1
        args.slice! 0
      end

      table, key, content = args

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

          return "> Database '#{database}' doesn't exist\n- create it/" if e.message =~ /\ADatabase `.+` does not exist.$/

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
          result = "| Table is empty.  Create some docs?\n+ foo/\n+ bar/" if result.blank?

          return result
        rescue Exception=>e
          # If table didn't exist, prompt them to create
          return "| Table doesn't exist\n+ create it" if e.message =~ /\ATable `.+` does not exist.$/
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

        # "create it", so create table...

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

        # Get doc and display it...

        doc = r(database).table(table).get(key).run

        # Document doesn't exist, so create sample...

        doc ||= {"name"=>"not there", "description"=>"This doc doesn't exist yet!  Modify and expand to create"}

        doc.delete "id"

        txt = JSON.pretty_generate doc
        # Remove { and }
        txt.sub! /\A{\n/, ''
        txt.sub! /\n}\z/, ''

        return Tree.pipe txt
      end

      # /table/key/content, so save...

      content = "{\n#{content}}"   # Put { and } back on

      doc = JSON[content]
      doc["id"] = key

      result = r(database).table(table).insert(doc, conflict:"replace").run

      "<! saved!"

    end

  end

end
