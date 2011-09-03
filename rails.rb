require "ruby_console"

class Rails
  extend ElMixin

  CODE_SAMPLES = %q<
    # Show options to help create new rails app
    - Show options: Rails.menu
  >

  def self.menu name=nil, port=nil
    if name.nil?  # If no project, list them
      r = ["current dir: #{View.dir}, 3000/"]
      r << (Projects.listing.map{|k, v| "#{k}: #{v}/"}.sort)
      r << "upper: #{View.dir_of_after_bar}, 3000/" if View.bar?
      return r.flatten#.join('')
    end

    # If project, list options
    puts "
      + .create
      - .start
      - .url '/'
      + .controller(:bar, :index)
      + .migration(:bar)
      - .shell
      - .console
      + .dirs/
      "
    #         - .snippets

  end

  def self.create dir, port=nil
    puts "rails -s #{dir}"
    puts Console.run("rails -s #{dir}", :sync => true)
  end

  def self.controller controller, action, dir, port=nil
    puts Console.run("script/generate controller #{controller} #{action}", :dir => dir, :sync => true)
  end

  def self.migration name, dir, port=nil
    puts Console.run("script/generate migration #{name}", :dir => dir, :sync => true)
  end

  def self.console dir, port=nil
    Console.run "script/console", :dir => dir, :buffer => "*#{dir} console"
  end

  def self.shell dir, port=nil
    Console.run "", :dir => dir, :buffer => "*#{dir} shell"
  end

  def self.dirs dir, port=3000
    puts "
      + #{dir}/
      "
  end

  def self.start dir, port=3000
    port.sub! /\/$/, ''
    if View.in_bar?
      buffer = View.buffer
      View.to_after_bar
      View.to_buffer buffer
    end
    port = port ? "-p #{port}" : ""
    Console.run "script/server #{port}", :dir => dir, :buffer => "*rails #{dir}"
  end

  def self.url path, dir=nil, port=3000
    port.sub! /\/$/, ''
    browse_url "http://localhost:#{port}#{path}"
    nil
  end

  def self.models model=nil, option=nil
    if model.nil?  # If no model, show all
      RubyConsole[:rails].run("Dir.glob(RAILS_ROOT + '/app/models/**/*.rb').each { |file| require file }")
      puts RubyConsole[:rails].run("puts Object.subclasses_of(ActiveRecord::Base).map{|m| \"+ \#{m}/\"}").sort
      return
    end

    puts "
      + .first/
      + .by :id=>1/
      + .by :recent=>1/
      + .count/
      + .associations/
      + .columns/
      - .source
      "
  end

  def self.first model
    model.sub! /\/$/, ''
    RubyConsole[:rails].run("y #{model}.find(:first)").strip.gsub(/^/, '| ')
  end

  def self.by options, model, ident=nil, content=nil
    model.sub! /\/$/, ''

    field, val = options.to_a.first

    if ident.nil?   # If no record passed, just show all
      result = RubyConsole[:rails].run(%Q[
        columns = #{model}.columns.map {|c| c.name}
        if #{field == :recent}   # If it's :recent, handle it specially
          date = columns.member?("updated_at") ? "updated_at" : "id"   # Find best ..._at field
          all = #{model}.find(:all, :conditions => "\#{date} IS NOT NULL", :order => "\#{date} desc", :limit => #{val})
        else
          all = #{model}.find(:all, :conditions=>'#{field}=#{val.inspect}')
        end
        has_name = all.first.has_attribute?(:name)
        all.each {|r| puts "- \#{has_name ? r.name : 'id'}: \#{r.id}/" }
      ])
      return result
    end

    if content.nil?   # If no content passed, just show one
      # Display 1 result
      result = RubyConsole[:rails].run("y #{model}.find(#{ident.inspect})")
      return result.strip.gsub(/^/, '| ')
    end

    # Content passed, so save
    result = RubyConsole[:rails].run(%Q[
      content = #{content.inspect}
      hash = YAML::load(content).attributes

      model = #{model}.find_or_create_by_id hash['id']
      response = model.update_attributes hash
      puts response ? "saved!" : "error!"
    ])

    "- #{result}"

  end



  def self.count model
    model.sub! /\/$/, ''
    count = RubyConsole[:rails].run("puts #{model}.count")
    "- #{count}"
  end

  def self.associations model
    model.sub! /\/$/, ''
    result = RubyConsole[:rails].run("#{model}.reflect_on_all_associations.each {|a| puts \"\#{a.macro} \#{a.name}\"}")
    result.strip.gsub(/^/, '- ')
  end


  def self.columns model
    model.sub! /\/$/, ''

    puts RubyConsole[:rails].run(%Q[
      puts #{model}.columns.map {|c| "- \#{c.type}: \#{c.name}"}.sort
      ])
  end




  def self.source_open model
    View.to_after_bar
    model = TextUtil.snake_case(model).gsub('::', '/')
    View.open("$tr/app/models/#{model}.rb")
  end

  def self.source model
    model.sub! /\/$/, ''
    model = TextUtil.snake_case(model).gsub('::', '/')
    View.open Bookmarks["$rails/app/models/#{model}.rb"] rescue nil
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

    View.insert FileTree.paths_to_tree(
      (c + v).sort.uniq )

    View.to_top
    Move.to_junior
  end

end

Keys.ORM { CodeTree.display_menu("- Rails.models/") }   # Open Rails Models
# Keys.enter_list_models {   # Enter Rails Models
#   $el.insert "- Rails.models/"
#   $el.open_line 1
#   CodeTree.launch
# }

if RubyConsole[:rails].nil? && Bookmarks['$rails']
  RubyConsole.register(:rails, "cd #{Bookmarks['$rails']}; script/console")  # Do this only once
end

