class Mysql
  def self.menu
    "
    + .tables/
    + .dbs/
    - .create_db 'foo'
    - .drop_db 'foo'
    "
  end

  def self.default_db db
    @default_db = db
  end

  def self.tables table=nil
    if ! @default_db
      if ! table
        return "- Use which db by default?: #{Mysql.dbs[1].sub('/', '')}"
        #         return "- Use which db by default?: foo"
      end
      @default_db = table
      return "| Default db set temporarily. Add this\n| to a .rb file to make permenant:\n|\n|   Mysql.default_db '#{table}'\n|"
    end
    #     return "- Set the default db first:\nMysql.default_db 'foo_db'" if ! @default_db

    self.dbs @default_db, table
  end

  def self.dbs db=nil, table=nil

    db.sub! /\/$/, '' if db
    table.sub! /\/$/, '' if table

    if table
      # Whole table
      if ! Keys.prefix_u
        sql = "select * from #{table} limit 1000"
        return self.run(db, sql).gsub(/^/, '| ')
      else
        # Get fields to use
        fields = self.run db, "select * from #{table} limit 1"
        fields = fields.sub(/\n.+/m, '').split("\t")
        fields &= ['id', 'slug', 'name', 'partner_id']
        fields = ['*'] if fields.blank?
        sql = "select #{fields.join ', '} from #{table} limit 1000"
        txt = Mysql.run(db, sql)

        return txt.gsub(/^/, '| ')
      end
    end

    if db
      return Mysql.run(db, 'show tables').split[1..-1].map{|o| "#{o}/"}
    end

    Mysql.run(nil, 'show databases').split[1..-1].map{|o| "#{o}/"}
  end

  def self.create_db name
    Console.run "mysqladmin -u root create #{name}", :buffer => "create #{name}"
  end

  def self.drop_db name
    Console.run "mysqladmin -u root drop #{name}", :buffer => "drop #{name}"
  end

  def self.run db, sql
    File.open("/tmp/tmp.sql", "w") { |f| f << sql }
    Console.run "mysql -u root #{db} < /tmp/tmp.sql", :sync=>true
  end
end

Keys.enter_list_mysql { CodeTree.insert_menu('- Mysql.dbs/') }

