gem 'couchrest'
require 'couchrest'

class Couch
  @@server = 'http://localhost:5984'

  def self.menu name=nil
    if name.nil?   # If no name passed
      return (["- .start\n"] +
        (View.buffer_open?("*couchdb") ? ["  - buffer: **couchdb\n"] : []) +
        ["- .admin_url\n", "+ .databases/\n"]).join('')
    end

  end

  def self.start
    buffer = '*couchdb'
    return View.message("*couchdb already open", :beep=>1) if View.buffer_open? buffer

    Console.run('sudo couchdb', :buffer=>buffer)
  end

  def self.admin_url
    View.url "#{@@server}/_utils"
    nil
  end

  def self.databases db=nil

    if db.nil?   # If no db yet, list all
      txt = Net::HTTP.get(URI.parse("#{@@server}/_all_dbs"))
      dbs = JSON[txt]

      # Unless C-u, only show _development dbs
      dbs = dbs.select{|o| o =~ /_development$/} unless Keys.prefix == :u

      return dbs.map{|i| "#{i}/"}
    end

    "
    + .docs/
    + .views/
    + .all_docs/
    + .delete/
    + .rest_tree/
    + .crud/
    "
  end

  def self.rest_tree db
    %Q[
    - GET #{@@server}/#{db}
      + create db: PUT
      + delete db: DELETE
      + all: _all_docs/
      - bulk add: _bulk_docs/
        POST
          {"docs":[
            {"_id":"a", "txt":"Aye"},
            {"_id":"b", "txt":"Bee"},
          ]}
      - bar/
        + PUT {\"txt\":\"hi\"}
        - ?rev=9123589
          + DELETE
      - _view/d1/v1/
      - _design/d1/
        + PUT {"views": {"v1": {"map": "function(doc){ emit(\\\"a\\\", null); }" }}}
    ]
  end

  def self.delete db, id=nil

    # If no id, show all id's
    if id.nil?
      all = RestTree.request 'GET', "#{@@server}/#{db}_all_docs", nil
      rows = JSON[all]['rows']
      return rows.map{|i| "#{i['id']}/"}
    end

    self.escape_slashes id

    # If id, look it up to get rev
    record = RestTree.request 'GET', "#{@@server}/#{db}#{id}", nil
    rev = JSON[record]['_rev']
    # Delete it
    RestTree.request 'DELETE', "#{@@server}/#{db}#{id}?rev=#{rev}", nil
  end

  def self.escape_slashes id
    # If id has multiple slashes, escape all but the last
    if id =~ /\/.+\/$/
      id.sub! /\/$/, ''   # Remove last slash
      id.gsub!('/', '%2F') unless id =~ /^_design/   # Escape slashes
      id.sub! /$/, '/'   # Put last back
    end
  end

  def self.docs db, id=nil, doc=nil
    db.sub! /\/$/, ''

    # If no id, show all id's
    if id.nil?
      all = RestTree.request 'GET', "#{@@server}/#{db}/_all_docs", nil
      rows = JSON[all]['rows']
      return rows.map{|i| "#{i['id']}/"}
    end

    self.escape_slashes id

    # If no doc, output doc
    if doc.nil?
      record = RestTree.request 'GET', "#{@@server}/#{db}/#{id}", nil
      return record.gsub("\\n", "\n").gsub(/^/, '|')
    end

    # Doc passed, so save it

    # If a record is found, add rev
    record = RestTree.request 'GET', "#{@@server}/#{db}/#{id}", nil
    if record !~ /404 /
      rev = JSON[record]['_rev']
      # Insert rev after first {, or replace if there already
      if doc =~ /"_rev":"\d+"/
        doc.sub! /("_rev":")\d+(")/, "\\1#{rev}\\2"
      else
        doc.sub! /\{/, "{\"_rev\":\"#{rev}\", "
      end
    end
    # Update it
    res = RestTree.request 'PUT', "#{@@server}/#{db}/#{id}", doc
    "|#{res}"
  end

  def self.views db
    db.sub! /\/$/, ''
    views = JSON[RestTree.request('GET', "#{@@server}/#{db}/_design/d1", nil)]['views'] rescue(return "- none found!")

    views.keys.each do |k|
      puts "y Cdb.#{db[/(.+)_/, 1]} :#{k}#, :key=>''"
    end
  end

  def self.all_docs db
    db = db[/(.+)_/, 1]
    puts "y Cdb.#{db}"
    puts "y Cdb.all :#{db}#, :key=>''"
    puts "- descending: y Cdb.all :#{db}, :skip=>1, :descending=>true"
  end

  def self.crud db
    db = db[/(.+)_/, 1]
    puts "y Cdb.#{db} ''"
    puts "- save: y Cdb.#{db}! '', 'txt'=>''"
    puts "- delete: y Cdb.delete :#{db}, ''"
    puts "- search: y Cdb.#{db} :startkey=>'b', :endkey=>'n', :skip=>2, :limit=>3"
    puts "- first: y Cdb.all :#{db}, :limit=>1, :include_docs=>true"
    puts "- last: y Cdb.all :#{db}, :skip=>1, :limit=>1, :descending=>true, :include_docs=>true"
  end

end
