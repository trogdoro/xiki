gem 'cassandra'
require 'xiki/core/cassandra'

require 'xiki/core/launcher'

module Xiki
  # Cassandras structure:
  #   keyspace ->* table family ->* key (->* super column) ->* column
  class CassandraDb

    @@connections = {}

    def self.conn keyspace
      @@connections[keyspace] ||= Cassandra.new(keyspace, '127.0.0.1:9160')
    end

    def self.insert keyspace, column_family, key, columns
      conn = self.conn keyspace
      conn.insert column_family, key, columns
    end

    def self.get keyspace, column_family, key, columns=nil
      conn = self.conn keyspace
      conn.get column_family, key, columns
    end

    def self.menu
      "
      + .start/
        - TODO: implement!
      + .statements/
        CassandraDb.insert 'Animals', :Ant, 'Steve', {'bbbb' => 'bbb'}
        CassandraDb.get 'Animals', :Ant, 'Steve'
      "
    end

  end
end
