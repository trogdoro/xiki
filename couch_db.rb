gem 'jchris-couchrest'
require 'couchrest'

class CouchDb
  @@server = 'http://localhost:5984'

  def self.menu name=nil
    if name.nil?   # If no name passed
      return (["- .start\n"] +
        (View.buffer_open?("*couchdb") ? ["  - buffer: **couchdb\n"] : []) +
        ["- .admin_url\n", "+ .databases/\n"]).join('')
    end

  end

  def self.start
    Console.run('sudo couchdb', :buffer=>'*couchdb')
  end

  def self.admin_url
    View.url "#{@@server}/_utils"
    nil
  end

  def self.databases db=nil

    if db.nil?   # If no db yet, list all
      txt = Net::HTTP.get(URI.parse("#{@@server}/_all_dbs"))
      return JSON[txt].map{|i| "#{i}/"}
    end

    %Q[
    + .docs/
    + .delete/
    + .rest_tree/
    ]
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
            {"_id":"b", "txt":"Bee"}
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

    record = RestTree.request 'GET', "#{@@server}/#{db}/#{id}", nil

    # If a record was found, add rev
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

end
