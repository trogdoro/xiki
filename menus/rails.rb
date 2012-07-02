require "ruby_console"

class Rails
  extend ElMixin

  CODE_SAMPLES = %q<
    # Show options to help create new rails app
    - Show options: Rails.menu
  >

  def self.menu
    "
    - .start/
    - .generate/
      - app/
      - model/
      - resource/
      - controller/
      - scaffold/
    - .interact/
      - .rails console/
      - .sqlite console/
      - .models/
    - .setup/
      - .db/
        - .migrate/
      - .use rspec/
    - .general/
      - .rails version/
    "
  end

  def self.menu_after txt, *args
    txt
  end

  def self.menu_before *path

    dir = Projects.current   # Returns dir in tree, or current project (top of projects.menu)

    # Don't intercede if already rails app or trying to generate
    return nil if ["generate", "general"].member?(path[0]) || File.exists?("#{dir}app")

    # If not a rails dir, give option to generate
    return "
      | No rails app in #{dir} yet.  Generate it?
      - generate/app/

      | Not project-specific
      - general/
      "
  end

  def self.rails_version
    "| #{`rails --version`}"
  end

  def self.use_rspec
    dir = Projects.current

    txt = "
      @ #{dir}
        - 1. Add these lines:
        - Gemfile
          |+group :development, :test do
          |+  gem 'rspec-rails'
          |+end
        |
        - 2. Run these commands:
        % bundle
        % rails g rspec:install
        |
        - 3. Delete the test/ dir:
        % rm -r test/
      "


  end


  def self.models
    dir = Projects.current
    dir = "#{dir}app/models/"

    entries = Dir.new(dir).entries.select{|o| o =~ /^\w/}
    entries.map{|o| "@r/#{TextUtil.camel_case o[/\w+/]}.first/"}.join("\n")
  end

  def self.sqlite_console
    Console.run "sqlite3 db/development.sqlite3", :dir=>Projects.current, :buffer=>"sqlite console"
    ".flash - opened console!"
  end

  def self.rails_console
    Console.run "rails c", :dir=>Projects.current, :buffer=>"rails console"
    ".flash - opened console!"
  end

  def self.generate what, name=nil, detail=nil

    examples = "
      > Example fields
      | id:primary_key
      | name:string
      | description:text
      | quantity:integer
      | price:decimal
      | purchased_at:datetime
      | delivery:boolean
      | user:references
      ".unindent

    case what
    when "app"
      Console.run "rails new . --skip-bundle", :dir=>Projects.current
      return "- generating rails app..."
    when "model", "resource", "scaffold"
      return View.prompt "Enter a name" if ! name
      return examples if ! detail
      fields = ENV['txt'].gsub("\n", ' ').strip
      Console.run "rails g #{what} #{name} #{fields}", :dir=>Projects.current
      return "- generating #{what}..."
    when "controller"
      return View.prompt "Enter a name" if ! name
      return View.prompt "Enter an action" if ! detail
      Console.run "rails g controller #{name} #{detail}", :dir=>Projects.current
      return "- generating controller..."
    end

    "- Don't know how to generate a '#{what}'!"
  end

  def self.start *args

    # If 1st arg is number, assume it's the port
    port = args[0] =~ /^\d+$/ ? args.shift : nil

    # If 'browse', just bring up in browser
    if args == ['browse']
      Firefox.url "http://localhost:#{port || 3000}/"
      return ".flash - opened in browser!"
    end

    command = "rails s"
    command << " -p #{port}" if port

    Console.run command, :dir=>Projects.current, :buffer=>"rails server"

    # Check whether it's already running
    "| Rails app was already running\n- browse/"
    "| Starting rails app...\n- browse/"

  end

  def self.command txt
    Console.run txt, :dir=>Projects.current
  end

  def self.migrate
    self.command "rake db:migrate"
  end

end
