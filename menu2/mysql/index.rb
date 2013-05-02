module Xiki::Menu

  class Mysql
    MENU_HIDDEN = "
      .tables/
      .dbs/
      "

    MENU = "
      > Todo > port
      - @tables/
      - @dbs/
      - @columns/
      - .setup/
        - .start/
        - .start in background/
        - stop/
          @ $ ps -eo pcpu,pid,user,args | grep mysql | grep -v grep
          @ $ kill _
        - db/
          - .use/
          - .create/
          - .drop/
        - table/
          - .create/
          - .drop/
        - .install/
        - config/
          > Main config file
          @ /etc/my.cnf

          > Example config files
          @ /usr/local/Cellar/mysql/5.5.25/support-files/
            - **cnf/
      - misc commands/
        > Drop db
        @ % mysqladmin -u root drop foo
        |
        > Others
        @ technologies/mysql/
      > Or just type some sql here
      | show tables
      "


    def self.menu_before *args
      return if args[0] !~ /^select /

      # select..., so take control

      options = yield

      self.select options, args
    end

    # TODO: bring these back
    #   def self.install
    #     "
    #     > Installing Mysql
    #     For now, this just has the mac / homebrew instructions.  Fork xiki on github to add docs for other platforms.

    #     > Install using homebrew
    #     - 1. double-click to install) @ % brew install mysql
    #     - 2. look at the output) and run the commands it tells you to run

    #     > More
    #     See this link for more info on installing:
    #     @http://www.mysql.com/downloads/mysql/
    #     "
    #   end

    #   def self.menu_after txt, *args
    #     return nil if txt
    #     ENV['no_slash'] = "1"
    #     Tree.quote self.run(nil, ENV['txt'])
    #   end

    #   def self.start
    #     Console.run "mysqld", :buffer=>"mysql", :dir=>"/tmp/"
    #     View.to_buffer "mysql"
    #     nil
    #   end

    #   def self.start_in_background
    #     Console.run "mysql.server start"
    #   end

    def self.default_db options
      conf = options[:conf]
      if(! conf || ! conf["default db"])
        raise "
          > No default db defined.  Define it?
          @conf/mysql/
          "
      end

      conf["default db"]
    end

    def self.tables *args
      db = self.default_db yield
      self.dbs db, *args
    end

    def self.dbs db=nil, table=nil, *row
      key, row = row[0] =~ /^\d+$/ ?
        row : [nil, row[0]]

      if row.is_a? Array
        key, row = row if row.is
      end

      # /, so list db's...

      if db.nil?
        txt = self.run('', 'show databases')
        return txt.split[1..-1].map{|o| "#{o}/"}
      end

      # /db/..., so just list the tables...

      if table.nil?
        txt = self.run(db, 'show tables')
        if txt.blank?
          return "> No tables exist.  Create one?\n- @mysql/setup/table/create/"
        end
        return txt.split[1..-1].map{|o| "#{o}/"}
      end

      # /db/table/id, so show one record...

      return self.db_by_key(db, table, key, row) if key

      # /db/table/..., so show all records...

      if row.nil?

        sql = "select * from #{table} limit 400"
        out = self.run(db, sql)

        out = "No records, create one?\n#{self.dummy_row(db, table)}" if out.blank?
        return Tree.quote out #.gsub(/^/, '| ')
      end

      # /db/table/row, so save...

      self.save db, table, row
      "@flash/- saved record!"

    end

    def self.db_by_key db, table, key, row

      # /db/table/key, so show single record as hash...

      if ! row
        # /db/table/row, so save...
        sql = "select * from #{table} where id = #{key}"
        row = self.run(db, sql)
        row = self.record_hash row
        return row.to_yaml.split("\n")[1..-1].join("\n").gsub(/^/, ': ')
      end

      # /db/table/key/row, so save...

      self.save db, table, row

      "@flash/- todo: save!"
    end

    # Returns key:value string to show record to the user
    def self.record_hash row
      keys, values = row.split("\n").map{|o| o.split("\t")}
      hash = {}
      keys.each_with_index{|key, i| hash[key] = values[i]}
      hash
    end

    #   def self.dummy_row db=nil, table=nil
    #     fields = self.fields db, table
    #     examples = {
    #       "int"=>"1",
    #       "varchar"=>"foo",
    #       "text"=>"bar bar",
    #       "date"=>"2011-01-01",
    #       "time"=>"2011-01-01",
    #       }
    #     fields = fields.map{|o| examples[o[1]]}
    #     fields.join("\t")
    #   end

    def self.fields db, table=nil
      txt = self.run db, "desc #{table}"
      txt.sub(/^.+\n/, '').split("\n").map{|o|
        l = o.split("\t")
        [l[0], l[1].sub(/\(.+/, '')] }
    end

    #   def self.use kind=nil, db=nil
    #     # If nothing passed, show db's

    #     if db.nil?
    #       return self.dbs
    #     end

    #     @default_db = db
    #     ".flash - using db #{db}!"
    #   end

    #   def self.create what, name=nil, columns=nil
    #     if name.nil?
    #       View.prompt "Type a name"
    #       return nil
    #     end

    #     if what == "db"
    #       txt = Console.run "mysqladmin -u root create #{name}", :sync=>true
    #       return ".flash - created db!"
    #     end

    #     if columns.nil?
    #       return "
    #         | id int not null auto_increment primary key,
    #         | name VARCHAR(20),
    #         | details text,
    #         | datestamp DATE,
    #         | timestamp TIME,
    #         "
    #     end

    #     txt = "
    #       CREATE TABLE #{name} (
    #         #{ENV['txt'].strip.sub(/,\z/, '')}
    #       );
    #       "

    #     out = self.run(@default_db, txt)

    #     ".flash - created table!"
    #   end

    #   def self.drop what, name=nil
    #     if name.nil?
    #       return what == "db" ? self.dbs : self.tables
    #     end

    #     if what == "db"
    #       txt = Console.run "mysqladmin -u root drop #{name}" #, :sync=>true
    #       return
    #     end

    #     out = self.run(@default_db, "drop table #{name}")

    #     ".flash - dropped table!"
    #   end

    def self.run db, sql

      db ||= self.default_db

      File.open("/tmp/tmp.sql", "w") { |f| f << sql }
      out = Console.run "mysql -u root #{db} < /tmp/tmp.sql", :sync=>true

      raise "> Mysql doesn't appear to be running.  Start it?\n- @mysql/setup/start/" if out =~ /^ERROR.+Can't connect/
      raise "| Database '#{db}' doesn't exist.  Create it?\n- @mysql/setup/db/create/#{$1}/" if out =~ /^ERROR.+Unknown database '(.+)'/
      raise "| Table doesn't exist.  Create it?\n- @mysql/setup/table/create/#{$1}/" if out =~ /^ERROR.+Table '.+\.(.+)' doesn't exist/
      raise Tree.quote(out) if out =~ /^ERROR/

      out
    end


    def self.save db, table, row
      if row =~ /\n/

        # Inspect row (k=>v\n...)...

        hash = YAML::load row
        txt = hash.map{|k, v| "#{k}=\"#{v}\"" }.join(", ")
      else
        fields = self.fields db, table

        # Normal row (tabs and no linebreaks)...

        row = row.sub(/^\| /, '').split("\t")
        txt = fields.map{|o| o[0]}.each_with_index.map{|o, i| "#{o}='#{row[i]}'"}.join(", ")
      end

      sql = "INSERT INTO #{table} SET #{txt} ON DUPLICATE KEY UPDATE #{txt}"
      self.run db, sql
    end

    def self.select options, args
      sql, row = args

      options[:no_slash] = 1
      default_db = self.default_db(options)

      # select..., so run and return results

      if ! row
        txt = self.run default_db, args[0]
        return txt.gsub /^/, '| '
      end

      table = sql[/from (.+?)( |$)/i, 1]
      Ol "table", table

      self.save default_db, table, row

      ".flash - TODO saved!"
    end

    # Launcher.add "columns" do |path|
    #   args = path.split('/')[1..-1]
    #   if args.size > 0
    #     next Mysql.run(@default_db, "desc #{args[0]}").gsub!(/^/, '| ')
    #   end
    #   Mysql.tables(*args)
    # end

    def self.def_patterns
      Xiki.def(/^select /) do |path, options|
        Xiki["mysql/#{options[:path]}"]
      end
    end

    # Maybe make the "default_conf" menu item be where it gets the
    # default conf
    def self.default_conf
      "
      > The db to use when none is specifed
      - default db: ?
      "
    end

end; end
