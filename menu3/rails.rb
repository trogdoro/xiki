require 'xiki/ruby_console'

class Rails

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
    - @models/
    - .setup/
      - .db/
        - .migrate/
      - .use rspec/
      - .rails version/
    "
    #     - .eval/
  end

  def self.menu_after txt, *args
    txt
  end

  def self.menu_before *path
    dir = Projects.default   # Returns dir in tree, or current project (top of projects.menu)

    # Don't intercede if already rails app or trying to generate
    return nil if ["generate", "general"].member?(path[0]) || File.exists?("#{dir}app")

    # If not a rails dir, give option to generate
    return "
      > No rails app in #{dir} yet.  Generate it?
      - generate/app/

      > Non project-specific options
      - general/
      "
  end

  def self.rails_version
    "| #{`rails --version`}"
  end

  def self.use_rspec
    dir = Projects.default

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

  def self.sqlite_console
    Console.run "sqlite3 db/development.sqlite3", :dir=>Projects.default, :buffer=>"sqlite console"
    ".flash - opened console!"
  end

  def self.rails_console
    Console.run "rails c", :dir=>Projects.default, :buffer=>"rails console"
    ".flash - opened console!"
  end

  def self.generate what, name=nil, detail=nil

    examples = "
      > Example fields
      | name:string
      | details:text
      | summary:text
      | quantity:integer
      | price:decimal
      | delivery:boolean
      | purchased_at:datetime
      | user:references
      ".unindent

    case what
    when "app"
      Console.run "rails new . --skip-bundle", :dir=>Projects.default
      return "- generating rails app..."
    when "model", "resource", "scaffold"
      return View.prompt "Enter a name" if ! name
      return examples if ! detail
      fields = ENV['txt'].gsub("\n", ' ').strip
      Console.run "rails g #{what} #{name} #{fields}", :dir=>Projects.default
      return "- generating #{what}..."
    when "controller"
      return View.prompt "Enter a name" if ! name
      return View.prompt "Enter an action" if ! detail
      Console.run "rails g controller #{name} #{detail}", :dir=>Projects.default
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

    Console.run command, :dir=>Projects.default, :buffer=>"rails server"

    # Check whether it's already running
    "| Rails app was already running\n- browse/"
    "| Starting rails app...\n- browse/"

  end

  def self.command txt
    Console.run txt, :dir=>Projects.default
  end

  def self.migrate
    self.command "rake db:migrate"
  end

  def self.eval *args

    if args.blank?
      return "
        > Put some code here, to run it in the context of a controller
        | request.methods
        "
    end

    # Text passed, so run put in controller method and call

    # Start server if necessary
      # And install the dev controller?

    txt = ENV['txt']

    "- TODO) implement calling dev_controller"
  end

  def self.run_in_app txt, options={}

    # If just code passed, run it...

    if options[:yaml]
      # If yaml passed, deduce code to save model, and run...

      txt = "
        txt = #{txt.inspect}
        mods = YAML::load(txt)

        mods = [mods] if ! mods.is_a?(Array)
        mods.each do |mod|
          mod.partial_updates = false

          existing = mod.class.where :id=>mod.id
          mod.instance_variable_set('@new_record', true) if existing.empty?

          mod.save
        end
        "
    end

    File.open("/tmp/rails_run_tmp.txt", "w") { |f| f << txt }
    response = HTTParty.get("http://localhost:3000/xiki_dev") rescue :exception

    return "| The rails server doesn't appear to be running.  Start default server?\n@rails/start/" if response == :exception

    return self.suggest_creating_controller if response.response.is_a?(Net::HTTPNotFound)

    return ".flash - saved!" if options[:yaml]

    txt = response.body
    txt.gsub!(/ +$/, '')

    # If is error, delete html at top
    txt.sub!(/.+?<h1>/m, '<h1>') if txt =~ /<div id="traces">/ && txt =~ /<h1>/

    # If file not found, suggest generating it
    if txt =~ /uninitialized constant XikiDevController::(\w+)/
      clazz = $1
      return "
        > Class '#{clazz}' doesn't exist.  Generate it as a model?
        @#{Projects.default}
          @rails/generate/model/#{clazz}
        "

    end

    txt

  end

  def self.suggest_creating_controller


    %`
    | This rails app may not have the xiki dev controller
    | installed to let xiki evaluate code in it in dev mode.
    | Create it?
    @#{Projects.default}
      - app/controllers/
        - xiki_dev_controller.rb
          | class XikiDevController < ApplicationController
          |   def index
          |     return render(:text=>"Disabled unless development and called locally.") if ! Rails.env.development? || request.remote_ip != "127.0.0.1"
          |     code = File.read "/tmp/rails_run_tmp.txt"
          |     txt = eval code
          |     render :text=>txt.to_yaml
          |   end
          | end
      - config/routes.rb
        |+  match 'xiki_dev' => 'xiki_dev#index'
        | end
    `
  end

end
