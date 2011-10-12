require "launcher"

class Merb

  def self.menu name=nil, port=nil
    unless name  # Print menu
      r = "+ current dir: #{View.dir}, 4000\n"
      r << "+ upper: #{View.dir_of_after_bar}, 4000\n" if View.in_bar?
      r + "
      + tmp: /tmp/merb1, 4000
      + .links/
      - .snippets/
      + .version/
      ".unindent
    else  # Print options
      "
      - .create
      - .start
      - .url '/'
      + .shells/
      + .generate/
      + .rake/
      + .models/
      + .db/
      + .files/
      "
    end
  end

  def self.shells dir, port=nil
    puts "
      - .console
      - .shell
      "
  end

  def self.files dir, port=nil
    puts "- #{self.path_from_dir(dir)}/"
  end

  def self.generate dir, port=nil
    puts "
      - .merb_gen 'controller posts'
      - .merb_gen 'model post body:string'
      - .merb_gen 'resource post body:string'
      "
  end

  def self.rake dir, port, task=nil
    unless task
      #puts self.path_from_dir(dir)
      out = Console.run "rake -T", :dir => self.path_from_dir(dir), :sync => true
      # Pull out tasks
      out.scan(/^rake ([\w:]+)/) do |m|
        puts "- #{m[0]}"
      end
      return
    end

    Console.run "rake #{task}", :dir => self.path_from_dir(dir), :buffer => "*rake #{dir}"

  end

  def self.merb_gen command, dir, port=nil
    Console.run "merb-gen #{command} --no-color", :dir => self.path_from_dir(dir), :buffer => "*merb-gen #{dir}"
  end

  def self.links
    puts "
      - home: http://www.merbivore.com/
      - open-source book: http://book.merbist.com/
      "
  end

  def self.db dir, port
    puts "
      - .db_create
      - .migrate
      "
  end

  def self.db_create dir, port
    Mysql.create_db self.name_from_dir(dir) + "_development"
  end

  def self.snippets
    puts "- create (o): Foo.create(:a => 'aa')"
    puts "- find first (o): y Foo.first"
  end

  def self.create dir, port=nil
    # Split into dir and name
    dir = self.path_from_dir(dir)
    path, name = dir.match(/(.+)\/(.+)/)[1..2]
    Console.run "merb-gen app -f --no-color #{name}", :dir=>path, :buffer=>"*merb-gen app #{name}" #, :sync=>true
  end

  def self.start dir, port
    Console.run "merb -a thin -p #{port}", :dir=>self.path_from_dir(dir), :buffer=>"*merb #{dir}"
    #     Console.run "merb -p #{port}", :dir=>self.path_from_dir(dir), :buffer=>"*merb #{dir}"
  end

  def self.shell dir, port
    Console.run "", :dir=>self.path_from_dir(dir)#, :buffer=>"*merb #{dir}"

    #     buffer = "*" + self.name_from_dir(dir) + " merb shell"

    #     path = self.path_from_dir(dir)
    #     View.to_after_bar

    #     # Just go there if already open
    #     if View.buffer_open? buffer
    #       View.to_buffer buffer
    #       return
    #     end

    #     Console.run nil, :path => path, :buffer => buffer
  end

#   def self.url path='/', dir=nil, port
#     $el.browse_url "http://localhost:#{port}#{path}"
#   end

  def self.url path, dir, port
    $el.browse_url "http://localhost:#{port}#{path}"
    nil
  end

  def self.console dir=nil, port=nil
    b = "*merb console #{self.name_from_dir(dir)}"
    if View.buffer_open?(b)
      View.handle_bar
      View.to_buffer b
    else
      Console.run "merb -i", :dir => dir, :buffer => b
    end
    nil
  end

  def self.migrate dir=nil, port=nil
    b = "*merb console #{self.name_from_dir(dir)}"

    if View.buffer_open?(b)
      View.handle_bar
      View.to_buffer b
    else  # If console not open, open it
      self.console dir
    end

    View.to_buffer(b)
    $el.insert("repository.auto_migrate!")
    #$el.insert("DataMapper::Base.auto_migrate!")
    Console.enter

    # TODO: item for migrating test
    #   - always take env as param?
    #     - default to 'development'
    #   - auto_migrate MERB_ENV=test
  end

  def self.name_from_dir dir
    dir = View.dir_of_after_bar if dir == "."
    dir[/(\w+)\/?$/, 1]
  end

  def self.path_from_dir dir
    # If ., grab path of window after bar
    dir = View.dir_of_after_bar if dir == "."
    Bookmarks.expand(dir, :absolute => true)
  end

  def self.models model=nil
    # If no model specified, show all
    unless model
      Dir.foreach(Bookmarks['$mo']) { |m|
        next unless m =~ /(.+)\.rb$/
        puts "+ #{TextUtil.camel_case($1)}/"
      }
      return
    end

    # Show options
    puts "
    + .first/
    + .recent 1/
    + .all/
    + .count/
    + .first \":name => 'foo'\"/
    + .recent 1, \":name => 'foo'\"/
    + .all \":name => 'foo'\"/
    + .count \":name => 'foo'\"/
    "
  end

  def self.first *args
    # If 2 args, it's where, model
    where = (args.size == 2) ? "(#{args.shift})" : ''
    model = args.shift
    model.sub! /\/$/, ''
    puts RubyConsole.at(:m, "y #{model}.first#{where}")
  end

  def self.all *args
    # If 2 args, it's where, model
    where = (args.size == 2) ? "(#{args.shift})" : ''
    model = args.shift
    model.sub! /\/$/, ''
    puts RubyConsole.at(:m, "y #{model}.all#{where}")
  end

  def self.count *args
    # If 2 args, it's where, model
    where = (args.size == 2) ? "(#{args.shift})" : ''
    model = args.shift
    model.sub! /\/$/, ''
    puts RubyConsole.at(:m, "puts #{model}.count#{where}")
  end

  def self.recent *args
    number = args.shift
    where = (args.size == 2) ? "#{args.shift}, " : ''
    model = args.shift
    model.sub! /\/$/, ''

    number ||= 1

    model.sub! /\/$/, ''
    puts RubyConsole.at(:m, "y #{model}.all(#{where}:order => [:updated_at.desc], :limit => #{number})")
  end

  def self.version
    Console.run('merb --version', :sync=>true)
  end

  def self.launch_merb_log_line line
    #     if Keys.prefix_u   # If U, just go there
    # Pull out action and controller
    action = line[/"action"=>"(.+?)"/, 1]
    controller = line[/"controller"=>"(.+?)"/, 1]
    # Open controller
    View.open "$co/#{controller}.rb"
    # Jump to method
    View.to_highest
    Search.forward "^\\s-+def #{action}[^a-z_]"
    Move.to_line_text_beginning

  end

  def self.init
    Launcher.add :paren=>"l4" do
      url = "http://localhost:4000/#{Line.without_label}"
      Keys.prefix_u ? $el.browse_url(url) : Firefox.url(url)
    end

    # Jump to controller from line in merb log
    Launcher.add(/^.* ~ Routed to: \{/) do |line|
      Merb.launch_merb_log_line line
    end

  end
end



Merb.init
