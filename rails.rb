class Rails
  extend ElMixin

  CODE_SAMPLES = %q<
    # Show options to help create new rails app
    - Show options: Rails.menu
  >

  def self.menu name=nil, port=nil
    unless name  # Print menu
      puts "+ name, port: /tmp/foo, 3001"
    else  # Print options
      puts "
        - .create
        - .start
        - .url
        - .controller(:bar, :index)
        - .migration(:bar)
        - .shell
        - .console
        - .dirs/
        - .snippets
        "
    end
  end

  def self.cr2 options={}
    # Did you pass me my ancestors?
    #puts "hhiiii"
    # Get name from parent
    parent = options[:ancestors][-1]
    name = parent[/:(\w+)/, 1]
    #insert name
    puts Shell.run("rails -s #{name}", :sync => true)
  end

  def self.create dir, port=nil
    puts "rails -s #{dir}"
    puts Shell.run("rails -s #{dir}", :sync => true)
  end

  def self.controller controller, action, dir, port=nil
    puts Shell.run("script/generate controller #{controller} #{action}", :dir => dir, :sync => true)
  end

  def self.migration name, dir, port=nil
    puts Shell.run("script/generate migration #{name}", :dir => dir, :sync => true)
  end

  def self.console dir, port=nil
    Shell.run "script/console", :dir => dir, :buffer => "*#{dir} console"
  end

  def self.shell dir, port=nil
    Shell.run "", :dir => dir, :buffer => "*#{dir} shell"
  end

  def self.dirs dir, port=3000
    puts "
      + #{dir}/
      "
  end

  def self.start dir, port=3000
    if View.in_bar?
      buffer = View.buffer
      View.to_after_bar
      View.to_buffer buffer
    end
    port = port ? "-p #{port}" : ""
    Shell.run "script/server #{port}", :dir => dir, :buffer => "*rails #{dir}"
  end

  def self.url dir=nil, port=3000
    browse_url "http://localhost:#{port}"
  end

  def self.snippets dir=nil, port=nil
    puts "
      - (o): y Account.find(:first)
      "
  end

  def self.models model=nil, option=nil
    unless model  # If no model, show all
      RubyConsole[:rails].run("Dir.glob(RAILS_ROOT + '/app/models/**/*.rb').each { |file| require file }")
      puts RubyConsole[:rails].run("puts Object.subclasses_of(ActiveRecord::Base).map{|m| \"\#{m}/\"}").sort
      return
    end

    unless option  # If no option, show options
      puts "
        first/
        recent 1/
        id: 10000/
        count/
        associations/
        columns/
        .source_file
        "
      return
    end

    model.sub! /\/$/, ''  # Remove / from model

    case option  # Show one model
    when 'first/'
      puts RubyConsole[:rails].run("y #{model}.find(:first)")
    when 'count/'
      puts RubyConsole[:rails].run("y #{model}.count")
    when /recent (\d+)\//
      limit = $1
      puts RubyConsole[:rails].run(%Q[
        columns = #{model}.columns.map {|c| c.name}
        date =  # Find best ..._at field
          if columns.member?("updated_at")
            "updated_at"
          else  # Most recent, using the id
            "id"
          end
        y #{model}.find(:all, :conditions => "\#{date} IS NOT NULL", :order => "\#{date} desc", :limit => #{limit.to_i})
        ])
    when '5'
      puts RubyConsole[:rails].run("y #{model}.find(:first)")
    when 'associations/'
      puts RubyConsole[:rails].run("#{model}.reflect_on_all_associations.each {|a| puts \"\#{a.macro} \#{a.name}\"}")
    when 'columns/'
      puts RubyConsole[:rails].run(%Q[
        puts #{model}.columns.map {|c| "- \#{c.type}: \#{c.name}"}.sort
        #puts #{model}.columns.map {|c| c.inspect}
        ])
    when /^(.+: .+)\/$/
      where = $1.gsub(': ', '=')
      puts RubyConsole[:rails].run("y #{model}.find(:all, :conditions => \"#{where}\")")
    else
      puts "- unknown option!"
    end
  end

  def self.source_open model
    View.to_after_bar
    model = TextUtil.snake_case(model).gsub('::', '/')
    View.open("$tr/app/models/#{model}.rb")
  end

  def self.source_file model
    model = TextUtil.snake_case(model).gsub('::', '/')
    puts "#{Bookmarks['$tr']}app/models/\n  #{model}.rb\n"
  end

  def self.tree_from_log

    orig = View.buffer
    set_buffer "*tail of /projects/rsam/trunk/log/development.log"
    txt = View.txt
    txt.sub!(/.+^(Processing )/m, "\\1")   # Delete except for last Processing...
    View.to_buffer orig

    c = txt.select{|l| l =~ /^Processing (\w+)#\w+ \(/}
    c = c.collect{|l|
      c, m = l.match(/^Processing (\w+)#(\w+) \(/)[1..2]
      c = TextUtil.snake_case(c)
      c = "/projects/rsam/trunk/app/controllers/#{c}.rb"
      "#{c}|  def #{m}"
    }

    v = txt.select{|l| l =~ /^Render\w+ [\w\/]+\//}
    v = v.collect{|l|
      l = l[/[\/\w]+\/[\/\w]+/]
      l = l.sub(/^\//, '')
      "/projects/rsam/trunk/app/views/#{l}.rhtml"
    }
    View.to_buffer "* Open List Log", :clear => true
    View.clear
    Notes.mode

    View.insert TreeLs.paths_to_tree(
      (c + v).sort.uniq )

    View.to_top
    Move.to_junior
  end

end

Keys.ORM { CodeTree.display_menu("Rails.models") }   # Open Rails Models
Keys.ERM {   # Enter Rails Models
  $el.insert "- Rails.models/"
  $el.open_line 1
  CodeTree.launch
}

unless RubyConsole[:rails]
  RubyConsole.register(:rails, "cd #{Bookmarks['$tr']}; script/console", "#{ENV['USERNAME']}@localhost:22")  # Do this only once
end
