class Postgres

  def self.menu db=nil, table=nil

    # If just /, show all db's...

    if db.nil?
      txt = Console.run("psql -c '\\l'", :sync=>true)
      txt = txt.scan(/^ (\w+)/).map{|i| "- #{i[0]}/"}
      return txt.join "\n"
    end

    # If /mydb/, show tables...

    if table.nil?

      txt = Console.run("psql #{db} -c '\\d'", :sync=>true)
      txt = txt.split "\n"
      txt = txt[3..-2]
      output = ""
      txt.each{|o|
        columns = o.split(/ *\| */)
        next if columns[2] != "table"
        output << "- #{columns[1]}/\n"
      }

      return output
    end

    # If /mydb/mytable, show rows...

    txt = Console.run("psql #{db} -c 'select * from #{table}'", :sync=>true)
    txt = txt.split "\n"
    header = txt.shift
    txt.shift
    txt.pop
    txt.unshift header.strip
    txt.map!{|o| "|#{o}"}
    txt.join("\n")

  end

  def self.create_db name
    Console.run "createdb #{name}"
  end

  def self.drop_db name
    Console.run "dropdb #{name}"
  end

  def self.tables db
    db.sub! /\/$/, ''
    txt = Console.run "psql #{db} -c '\\d'", :sync=>true
  end

end
