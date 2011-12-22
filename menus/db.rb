class Db
  def self.create db
    Couch.create db
  end

  def self.menu db=nil, *args
    # If no args, output list
    return Couch.databases if db.nil?

    return Couch.docs db, *args
  end
end
