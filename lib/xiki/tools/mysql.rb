module Xiki
  class Mysql
    def self.menu
      "
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
    end

    def self.install
      "
      > Installing Mysql
      For now, this just has the mac / homebrew instructions.  Fork xiki on github to add docs for other platforms.

      > Install using homebrew
      - 1. double-click to install) @ % brew install mysql
      - 2. look at the output) and run the commands it tells you to run

      > More
      See this link for more info on installing:
      @http://www.mysql.com/downloads/mysql/
      "
    end

    def self.menu_after txt, *args
      return nil if txt
      ENV['no_slash'] = "1"
      Tree.quote self.run(nil, ENV['txt'])
    end

    def self.default_db db
      @default_db = db
    end

    def self.start
      Console.run "mysqld", :buffer=>"mysql", :dir=>"/tmp/"
      View.to_buffer "mysql"
      nil
    end

    def self.start_in_background
      Console.run "mysql.server start"
    end

    def self.tables *args
      if ! @default_db && ! args[0]
        @default_db = "dev"   # Temp
      end

      self.dbs @default_db, *args
    end

    def self.dbs db=nil, table=nil, row=nil

      # If nothing passed, list db's

      if db.nil?
        txt = Mysql.run('', 'show databases')
        return txt.split[1..-1].map{|o| "#{o}/"}
      end

      db.sub! /\/$/, '' if db
      table.sub! /\/$/, '' if table

      # If just db passed, list the tables

      if table.nil?
        txt = Mysql.run(db, 'show tables')
        if txt.blank?
          @default_db = db
          return "> No tables exist.  Create one?\n- @mysql/setup/table/create/"
        end
        return txt.split[1..-1].map{|o| "#{o}/"}
      end

      # If table passed, so show all records

      if row.nil?
        # Whole table

        if ! $el || ! Keys.prefix_u
          sql = "select * from #{table} limit 400"
          out = self.run(db, sql)
          out = "No records, create one?\n#{self.dummy_row(db, table)}" if out.blank?
          return Tree.quote out #.gsub(/^/, '| ')
        else
          # Pick out just a few fields
          fields = self.run db, "select * from #{table} limit 1"
          fields = fields.sub(/\n.+/m, '').split("\t")
          fields &= ['id', 'slug', 'name', 'partner_id']
          fields = ['*'] if fields.blank?
          sql = "select #{fields.join ', '} from #{table} limit 400"
          txt = Mysql.run(db, sql)

          return txt.gsub(/^/, '| ')
        end
      end

      # Row passed, so save

      self.save db, table, row

      ".flash - saved record!"
    end

    def self.dummy_row db=nil, table=nil
      fields = self.fields db, table
      examples = {
        "int"=>"1",
        "varchar"=>"foo",
        "text"=>"bar bar",
        "date"=>"2011-01-01",
        "time"=>"2011-01-01",
        }
      fields = fields.map{|o| examples[o[1]]}
      fields.join("\t")
    end

    def self.fields db, table=nil
      txt = self.run db, "desc #{table}"
      txt.sub(/^.+\n/, '').split("\n").map{|o|
        l = o.split("\t")
        [l[0], l[1].sub(/\(.+/, '')] }
    end

    def self.use kind=nil, db=nil
      # If nothing passed, show db's

      if db.nil?
        return Mysql.dbs
      end

      @default_db = db
      ".flash - using db #{db}!"
    end

    def self.create what, name=nil, columns=nil
      if name.nil?
        View.prompt "Type a name"
        return nil
      end

      if what == "db"
        txt = Console.run "mysqladmin -u root create #{name}", :sync=>true
        return ".flash - created db!"
      end

      if columns.nil?
        return "
          | id int not null auto_increment primary key,
          | name VARCHAR(20),
          | details text,
          | datestamp DATE,
          | timestamp TIME,
          "
      end

      txt = "
        CREATE TABLE #{name} (
          #{ENV['txt'].strip.sub(/,\z/, '')}
        );
        "

      out = self.run(@default_db, txt)

      ".flash - created table!"
    end

    def self.drop what, name=nil
      if name.nil?
        return what == "db" ? Mysql.dbs : Mysql.tables
      end

      if what == "db"
        txt = Console.run "mysqladmin -u root drop #{name}" #, :sync=>true
        return
      end

      out = self.run(@default_db, "drop table #{name}")

      ".flash - dropped table!"
    end

    def self.run db, sql
      db ||= @default_db

      File.open("/tmp/tmp.sql", "w") { |f| f << sql }
      out = Console.run "mysql -u root #{db} < /tmp/tmp.sql", :sync=>true

      raise "> Mysql doesn't appear to be running.  Start it?\n- @mysql/setup/start/" if out =~ /^ERROR.+Can't connect/
      raise "| Select a db first:\n- @mysql/setup/db/use/" if out =~ /^ERROR.+: No database selected/
      raise "| Database doesn't exist.  Create it?\n- @mysql/setup/db/create/#{$1}/" if out =~ /^ERROR.+Unknown database '(.+)'/
      raise "| Table doesn't exist.  Create it?\n- @mysql/setup/table/create/#{$1}/" if out =~ /^ERROR.+Table '.+\.(.+)' doesn't exist/
      raise Tree.quote(out) if out =~ /^ERROR/
      out
    end

    def self.save db, table, row
      fields = self.fields db, table
      row = row.sub(/^\| /, '').split("\t")
      txt = fields.map{|o| o[0]}.each_with_index.map{|o, i| "#{o}='#{row[i]}'"}.join(", ")
      self.run db, "INSERT INTO #{table} SET #{txt} ON DUPLICATE KEY UPDATE #{txt}"
    end

    def self.select statement, row=nil
      table = statement[/from (.+?)( |$)/i, 1]

      # If just statement, run it

      if row.nil?
        txt = Mysql.run nil, statement.sub(/\/$/, '')
        txt = "No records, create one?\n#{self.dummy_row(@default_db, table)}" if txt.blank?
        return Tree.quote txt
      end

      # Row passed, so save it

      ENV['no_slash'] = "1"
      self.save @default_db, table, row

      ".flash - saved!"
    end
  end
end
