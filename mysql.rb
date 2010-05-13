#   $ /usr/local/Cellar/mysql/5.1.45/bin/mysql -u root -p "" -e "SELECT VERSION();"

class Mysql
  def self.menu
    puts "
      + foo/
        - .create_db
        - .drop_db
      "
  end

  def self.create_db name
    Console.run "mysqladmin -u root create #{name}", :buffer => "create #{name}"
  end

  def self.drop_db name
    Console.run "mysqladmin -u root drop #{name}", :buffer => "drop #{name}"
  end

# Todo
#CREATE USER chase IDENTIFIED BY 'chase';
end

