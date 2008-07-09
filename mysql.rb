class Mysql
  def self.menu
    puts "
      + foo/
        - .create_db
        - .drop_db
      "
  end

  def self.create_db name
    Shell.run "mysqladmin -u root create #{name}", :buffer => "create #{name}"
  end

  def self.drop_db name
    Shell.run "mysqladmin -u root drop #{name}", :buffer => "drop #{name}"
  end

# Todo
#CREATE USER chase IDENTIFIED BY 'chase';
end

