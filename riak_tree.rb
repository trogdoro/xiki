gem 'httparty'; require 'httparty'
gem 'json'; require 'json'
gem 'riak-client'; require 'riak'

module Riak

  RIAK_URL = "http://127.0.0.1:8098"

  def self.menu
    [ ".buckets/",
      ".index/",
      ".filter/",
      ".get 'foo/bar'",
      ".help/",
      ".log/",
      ".ping/",
    ]
  end

  def self.buckets bucket=nil, key=nil, txt=nil


    if key
      key.sub! /\/$/, ''
      key.gsub! '/', '%2F'
      key.sub! /^"(.*)"$/, "\\1"
    end

    bucket.sub! /\/$/, '' if bucket

    if txt
      # If 0, delete
      if Keys.prefix == 0
        self.delete "#{bucket}/#{key}"
        return "- deleted!"
      end

      # Save text if txt passed
      txt = CodeTree.siblings :as_string=>true

      data = YAML::load(txt)

      self.put "#{bucket}/#{key}", YAML::load(txt)

      return "- saved!"
    end

    # Show contents of object if passed
    if key
      Files.append "~/.emacs.d/riak_log.notes", "- Riak.buckets \"#{bucket}\", \"#{key}\"/"

      key = "#{bucket}/#{key}"

      begin
        return self.get_hash(key).to_yaml.sub(/^--- \n/, '').gsub(/^/, '| ')
      rescue Exception=>e
        return self.get_hash(key, :raw=>true).gsub(/^/, '| ')
      end
    end
    # Show contents of bucket if passed
    return self.get_hash("#{bucket}?keys=true")['keys'].sort.map{|o| "#{o}/"} if bucket
    # Nothing passed, so list buckets
    self.get_hash("?buckets=true")["buckets"].sort.map{|o| "#{o}/"}
  end

  def self.filter bucket=nil, *args
    bucket.sub! /\/$/, '' if bucket

    if args.any?
      args = args.join(', ').sub(/\/$/, '')

      Files.append "~/.emacs.d/riak_log.notes", "- Riak.filter \"#{bucket}\", #{args}/"

      args = eval "[#{args}]"

      results = Riak::MapReduce.new(Riak::Client.new).
        add(bucket, args).
        map("function(v){ return [v.key]; }", :keep => true).
        run

      return results.sort.reverse.to_yaml.sub(/\A--- \n/, '').gsub(/^/, '| ')
    end

    if bucket   # Examples
      return ["['matches', 'b']/", "['starts_with', 'a']/", "['between', 'a', 'b']/", "['or', [['eq', 'a']], [['less_than', 'g']]]/", "['tokenize', '-', 1], ['eq', 'basho']/"]
    end

    self.buckets   # Delegate to .buckets to list buckets

  end

  def self.index bucket=nil, search=nil

    if search
      results = self.get_hash "#{bucket}/index/#{search}", :root=>'buckets'#, :raw=>'true'
      return results.to_yaml.gsub(/^/, '| ')
    end

    if bucket   # Examples
      return ["zip_bin/eq/43215/", "zip_bin/range/40000/49999/"]
    end

    self.buckets   # Delegate to .buckets to list buckets
  end

  def self.get path, txt=nil

    if txt
      txt = CodeTree.siblings :as_string=>true
      self.put path, YAML::load(txt)
      return "- saved!"
    end

    hash = self.get_hash(path).to_yaml.sub("--- \n", '').gsub(/^/, '| ')

  end

  def self.get_hash path, options={}
    begin
      response = HTTParty.get("#{RIAK_URL}/#{options[:root]||'riak'}/#{path}")
    rescue Exception=>e
      raise "Riak not running?"
    end

    return response.code if response.code.to_s !~ /^2/
    if options[:raw]
      response.body
    else
      result = ::JSON[response.body]
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

  def self.put path, options={}
    Ol << "path: #{path.inspect}"
    # If no body passed in, wrap necessary stuff around it
    if options[:body].nil?
      Ol.line
      options = {
        :body=>options.to_json,
        :headers=>{'Content-Type'=>'application/json'},
      }
    end
    Ol << "#{RIAK_URL}/riak/#{path}"
    Ol << "options: #{options.inspect}"

    HTTParty.put("#{RIAK_URL}/riak/#{path}", options)
    nil
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

  def self.ping
    `#{Bookmarks['$riak']}/bin/riak ping`
  end

  def self.log
    txt = File.read File.expand_path("~/.emacs.d/riak_log.notes")
    txt = txt.split("\n").reverse.uniq.join("\n")
  end

end

Keys.enter_list_riak do
  CodeTree.insert_menu '- Riak.menu/'
end

Keys.enter_list_buckets do
  CodeTree.insert_menu '- buckets/'
end

Launcher.add(:paren=>'r') do
  line = Line.without_label
  Tree.under(Riak.get_hash(line).to_yaml.sub(/^--- \n/, ''))
end

LineLauncher.add(:paren=>'rr') do   # Get raw json version
  line = Line.without_label
  Tree.under(Riak.get_hash(line, :raw=>true))
end

Launcher.add("riak") do |path|
  "
  - @buckets/
  "
end

Launcher.add "buckets" do |path|
  args = path.split('/')[1..-1]
  Riak.buckets(*args)
end

# Keys.open_riak_log { CodeTree.display_menu("- Riak.log/") }
Keys.enter_as_riak { CodeTree.insert_menu("- Riak.log/") }

CodeTree.add_menu "Riak"   # Force it, since we're a module
