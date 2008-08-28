require 'view'

class Merb
#   extend AutoMenu

  def self.menu name=nil, port=nil
    unless name  # Print menu
      puts "
        + name, port: ., 4000
        + .links/
        - .snippets
        + .models/
        "
    else  # Print options
      puts "
        - .create
        - .start
        - .url '/'
        + .shells/
        + .generate/
        + .rake/
        + .db/
        + .dirs/
        "
    end
  end

  def self.shells dir, port=nil
    puts "
      - .console
      - .shell
      "
  end

  def self.dirs dir, port=nil
    puts self.path_from_dir(dir) + "/"
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
      out = Shell.run "rake -T", :dir => self.path_from_dir(dir), :sync => true
      # Pull out tasks
      out.scan(/^rake ([\w:]+)/) do |m|
        puts m[0]
      end
      return
    end

    Shell.run "rake #{task}", :dir => self.path_from_dir(dir), :buffer => "*rake #{dir}"

  end

  def self.merb_gen command, dir, port=nil
    Shell.run "merb-gen #{command}", :dir => self.path_from_dir(dir), :buffer => "*merb-gen #{dir}"
  end

  def self.links
    puts "
      - open-source book: http://merb.4ninjas.org/
      - mattetti recommended: http://www.socialface.com/slapp/
      - Just in Time: http://jit.nuance9.com/2007/11/merb-datamapper-getting-rolling.html
      - Keith found: http://paulbarry.com/articles/2007/11/16/merb-and-data-mapper
      - Merborial: http://rorblog.techcfl.com/2008/02/01/merborial-getting-started-with-merb-and-datamapper/
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

    Shell.run "merb-gen app #{name}", :dir => path, :buffer => "*merb-gen app #{name}"
  end

  def self.start dir, port
    Shell.run "merb -p #{port}", :dir => self.path_from_dir(dir), :buffer => "*merb #{dir}"
  end

  def self.shell dir, port
    buffer = "*" + self.name_from_dir(dir) + " merb shell"

    path = self.path_from_dir(dir)
    View.to_after_bar

    # Just go there if already open
    if View.buffer_open? buffer
      View.to_buffer buffer
      return
    end

    Shell.run nil, :path => path, :buffer => buffer
  end

#   def self.url path='/', dir=nil, port
#     $el.browse_url "http://localhost:#{port}#{path}"
#   end

  def self.url path, dir, port
    $el.browse_url "http://localhost:#{port}#{path}"
  end

  def self.console dir=nil, port=nil
    b = "*merb console #{self.name_from_dir(dir)}"
    if View.buffer_open?(b)
      View.handle_bar
      View.to_buffer b
    else
      Shell.run "merb -i", :dir => dir, :buffer => b
    end
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
    Shell.enter

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
    puts RubyConsole.at(:ml, "y #{model}.first#{where}")
  end

  def self.all *args
    # If 2 args, it's where, model
    where = (args.size == 2) ? "(#{args.shift})" : ''
    model = args.shift
    model.sub! /\/$/, ''
    puts RubyConsole.at(:ml, "y #{model}.all#{where}")
  end

  def self.count *args
    # If 2 args, it's where, model
    where = (args.size == 2) ? "(#{args.shift})" : ''
    model = args.shift
    model.sub! /\/$/, ''
    puts RubyConsole.at(:ml, "puts #{model}.count#{where}")
  end

  def self.recent *args
    number = args.shift
    where = (args.size == 2) ? "#{args.shift}, " : ''
    model = args.shift
    model.sub! /\/$/, ''

    number ||= 1

    model.sub! /\/$/, ''
    puts RubyConsole.at(:ml, "y #{model}.all(#{where}:order => [:updated_at.desc], :limit => #{number})")
  end

end
