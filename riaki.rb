gem 'httparty'; require 'httparty'
gem 'json'; require 'json'
gem 'riak-client'; require 'riak'

class Riaki

  RIAK_URL = "http://127.0.0.1:8098"

  def self.menu
    [ ".buckets/",
      "\.index/",
      ".filter/",
      ".help/",
      ]
  end

  def self.buckets bucket=nil, key=nil, txt=nil

    key.sub! /\/$/, '' if key; key.gsub! '/', '%2F' if key
    bucket.sub! /\/$/, '' if bucket

    if txt
      # If 0, delete
      if Keys.prefix == 0
        self.delete "#{bucket}/#{key}"
        return "- deleted!"
      end

      # Save text if txt passed
      txt = CodeTree.siblings :as_string=>true
      self.post "#{bucket}/#{key}", YAML::load(txt)
      return "- saved!"
    end

    # Show contents of object if passed
    if key
      key = "#{bucket}/#{key}"
      begin
        return self.get(key).to_yaml.sub(/^--- \n/, '').gsub(/^/, '| ')
      rescue Exception=>e
        return self.get(key, :raw=>true).gsub(/^/, '| ')
      end
    end

    # Show contents of bucket if passed
    return self.get("#{bucket}?keys=true")['keys'].map{|o| "#{o}/"} if bucket

    # Nothing passed, so list buckets
    self.get("?buckets=true")["buckets"].map{|o| "#{o}/"}
  end

  def self.filter bucket=nil, *args

    bucket.sub! /\/$/, '' if bucket

    if args.any?
      args = args.join(', ').sub(/\/$/, '')
      args = eval "[#{args}]"

      results = Riak::MapReduce.new(Riak::Client.new).
        add(bucket, args).
        map("function(v){ return [v.key]; }", :keep => true).
        run

      return results.to_yaml.gsub(/^/, '| ')
    end

    if bucket   # Examples
      return ["['matches', 'b']/", "['starts_with', 'a']/", "['between', 'a', 'b']/", "['or', [['eq', 'a']], [['less_than', 'g']]]/", "['tokenize', '-', 1], ['eq', 'basho']/"]
    end

    self.buckets   # Delegate to .buckets to list buckets

  end

  def self.index bucket=nil, search=nil
    #bucket.sub! /\/$/, '' if bucket

    if search
      results = self.get "#{bucket}/index/#{search}", :root=>'buckets'#, :raw=>'true'
      return results.to_yaml.gsub(/^/, '| ')
    end

    if bucket   # Examples
      return ["zip_bin/eq/43215/", "zip_bin/range/40000/49999/"]
    end

    self.buckets   # Delegate to .buckets to list buckets
  end

  def self.get path, options={}
    response = HTTParty.get("#{RIAK_URL}/#{options[:root]||'riak'}/#{path}")
    return response.code if response.code.to_s !~ /^2/
    if options[:raw]
      response.body
    else
      result = JSON[response.body]
      result['_link'] = response.headers['link'].split(', ') if Keys.prefix_u
      result
    end

  end

  def self.post path, options={}
    # If no body passed in, wrap necessary stuff around it
    if options[:body].nil?
      options = {
        :body=>options.to_json,
        :headers=>{'Content-Type'=>'application/json'},
      }
    end

    HTTParty.post("#{RIAK_URL}/riak/#{path}", options)
  end

  def self.delete path
    HTTParty.delete "#{RIAK_URL}/riak/#{path}"
  end

  def self.help bucket=nil, key=nil
    "
      | To save
      | - C-enter on object body
      |
      | To delete
      | - C-0, C-enter on object body
      ".unindent
  end

end

Keys.enter_list_riak do
  Keys.prefix == 0 ?
    CodeTree.insert_menu('- Riaki.menu/') :
    CodeTree.insert_menu('- Riaki.buckets/')
end

LineLauncher.add(:paren=>'riak') do
  line = Line.without_label
  FileTree.insert_under(Riaki.get(line).to_yaml.sub(/^--- \n/, ''))
end

LineLauncher.add(:paren=>'riakr') do   # Get raw json version
  line = Line.without_label
  FileTree.insert_under(Riaki.get(line, :raw=>true))
end
