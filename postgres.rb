class Postgres

  def self.name= name
    @@name = name
  end

  def self.menu name=nil

    # If no name, list all names
    if name.nil?
      return Shell.run("psql -c '\\l'", :sync=>true).scan(/^ (\w+)/).map{|i| "- #{i}/"}
    end

    puts "
      - .tables/
      - .create_db :foo
      - .drop_db :foo
      "
  end

  def self.create_db name
    Shell.run "createdb #{name}"
  end

  def self.drop_db name
    Shell.run "dropdb #{name}"
  end

  def self.tables db
    db.sub! /\/$/, ''
    txt = Shell.run "psql #{db} -c '\\d'", :sync=>true
  end

end
