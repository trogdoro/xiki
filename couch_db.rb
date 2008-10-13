class CouchDb
  def self.menu
    res = ['.start']
    # Show buffer it exists
    res << 'running: **couchdb' if View.buffer_open?('*couchdb')
    res << ['.admin_url', '.rest_url/']
    res.flatten
  end

  def self.start
    Console.run('sudo couchdb', :buffer=>'*couchdb')
  end

  def self.admin_url
    View.url 'http://localhost:5984/_utils'
    nil
  end

  def self.rest_url
    '
    GET http://localhost:5984/foo/
      bar'
  end

end
