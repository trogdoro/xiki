class Mysql
  def self.menu
    puts "
      + .dbs/
      + foo/
        - .create_db
        - .drop_db
      "
  end

  def self.dbs db=nil, table=nil
    db.sub! /\/$/, '' if db
    table.sub! /\/$/, '' if table

    if table
      sql = "select * from #{table} limit 1000"
      return Mysql.run(db, sql).gsub(/^/, '| ')
    end

    if db
      return Mysql.run(db, 'show tables').split.map{|o| "#{o}/"}
    end

    Mysql.run(nil, 'show databases').split.map{|o| "#{o}/"}
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
