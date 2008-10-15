class CouchDb
  @@server = 'http://localhost:5984'

  def self.menu name=nil
    if name.nil?   # If no name passed
      return "
      - .start
      #{View.buffer_open?("*couchdb") ? "  - running: **couchdb\n" : ''}\
      - .admin_url
      - .rest/
      "
    end

  end

  def self.start
    Console.run('sudo couchdb', :buffer=>'*couchdb')
  end

  def self.admin_url
    View.url "#{@@server}/_utils"
    nil
  end

  def self.rest db=nil

    if db.nil?   # If no db yet, list all
      txt = Net::HTTP.get(URI.parse("#{@@server}/_all_dbs"))
      return JSON[txt].map{|i| "#{i}/"}
    end

    %Q[
    - GET #{@@server}/#{db}
      + create db: PUT
      + delete db: DELETE
      + _all_docs/
      - bar/
        + PUT {\"txt\":\"hi\"}
      - _view/d1/v1/
      - _design%2Fd1/
        + PUT {"views": {"v1": {"map": "function(doc){ emit(\\\"a\\\", null); }" }}}
    ]
  end

end
