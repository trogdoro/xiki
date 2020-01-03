require 'xiki/core/ruby_console'

module Xiki
  class Rails

    CODE_SAMPLES = %q<
      # Show options to help create new rails app
      - Show options: Rails.menu
    >

    MENU_HIDDEN = "
      .running dir/
      .nest/
      "

    MENU = "
      - .start/
      - .generate/
        - app/
        - plugin/
        - model/
        - resource/
        - controller/
        - scaffold/
      - .inspect/
        - .routes/
      - .interact/
        - .rails console/
        - .sqlite console/
      - =models/
      - .setup/
        - .db/
          - .migrate/
        - .use rspec/
        - .rails version/
      "

    def self.nest
      Tree.to_parent
      Tree.collapse
      Line.delete
      txt = "
        /tmp/rails1/
          =rails/
        ".unindent

      View.<< txt, :dont_move=>1
      Line.next
      Line.to_beginning
      ""
    end


    def self.menu_before *path

      dir = Tree.closest_dir yield[:ancestors]


      return self.eval_in_app(path[0]) if path.length == 1 && path[0] =~ /\n/

      # Don't intercede if already rails app or trying to generate
      return nil if ["generate", "nest"].member?(path[0]) || File.exists?("#{dir}/app")

      # Not nested, so offer to nest...

      # Not a rails dir, so offer to nest under a dir...

      return "
        > Rails app doesn't exist.  Generate it?
        - generate/app/
        "
    end


    def self.eval_in_app code

      Ol "continue here!!"
      # Call localhost:3000/xikidev > to eval the code in the app

      txt = Rails.run_in_app code

    end


    def self.rails_version
      "| #{`rails --version`}"
    end

    def self.use_rspec
      dir = Tree.closest_dir yield[:ancestors]

      txt = "
        =#{dir}
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
      Shell.run "sqlite3 db/development.sqlite3", :dir=>self.dir(yield), :buffer=>"sqlite console", :dont_move=>1
      "<* opened console in other view!"
    end

    def self.rails_console
      Shell.run "rails c", :dir=>self.dir(yield), :buffer=>"rails console", :dont_move=>1
      "<* opened console in other view!"
    end

    def self.generate what, name=nil, detail=nil

      examples = "
        > 1. Example fields
        | name:string
        | details:text
        | summary:text
        | quantity:integer
        | price:decimal
        | delivery:boolean
        | purchased_at:datetime
        | user:references
        | --no-timestamps
        ".unindent

      dir = self.dir(yield)

      options = yield

      case what
      when "app"
        dir, stem = File.dirname(dir), File.basename(dir)

        Shell.run "rails new \"#{stem}\" --skip-bundle", :dir=>dir, :dont_move=>1
        return "- generating rails app in other view..."
      when "plugin"
        Shell.run "rails plugin new . --skip-bundle --full", :dir=>dir, :dont_move=>1
        return "- generating rails app in other view..."
      when "model", "resource", "scaffold"
        return View.prompt "Enter a name" if ! name
        return examples if ! detail
        fields = Tree.txt.gsub("\n", ' ').strip
        Shell.run "rails g #{what} #{name} #{fields}", :dir=>dir, :dont_move=>1
        dir = options[:ancestors][-1]
        return "
          | Generating #{what} in other view.  Now run the migrations:
          =#{dir}% rake db:migrate
          | Then restart the server
          =current/rails server
          | The url
          =http://localhost:3000/#{name}
          "
      when "controller"
        return View.prompt "Enter a name" if ! name
        return View.prompt "Enter an action" if ! detail
        Shell.run "rails g controller #{name} #{detail}", :dir=>dir, :dont_move=>1
        return "
          - Generating controller in other view...
          =http://localhost:3000/#{name}/#{detail}
          =#{dir}app/
            - controllers/#{name}_controller.rb
            - views/#{name}/#{detail}.html.erb
          "
      end

      "- Don't know how to generate a '#{what}'!"
    end

    def self.dir options={}

      # Nevermind > If no args, hit :3000 to get dir

      Tree.closest_dir options[:ancestors]
    end

    def self.start *args

      # If 1st arg is number, assume it's the port
      port = args[0] =~ /^\d+$/ ? args.shift : nil

      # If 'browse', just bring up in browser
      if args == ['browse']
        Firefox.url "http://localhost:#{port || 3000}/"
        return "<* opened in browser!"
      end

      command = "rails s"
      command << " -p #{port}" if port

      Shell.run command, :dir=>self.dir(yield), :buffer=>"rails server", :dont_move=>1

      # Check whether it's already running
      "| Rails app was already running\n- browse/"
      "| Starting rails app in other view...\n- browse/"

    end

    def self.command txt, options
      Shell.run txt, :dir=>self.dir(options)
    end

    def self.migrate
      self.command "rake db:migrate", yield
    end

    def self.routes
      self.command "rake routes", yield
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

      txt = Tree.txt

      "- TODO) implement calling dev_controller"
    end

    def self.run_in_app txt, options={}

      # If just code passed, run it...

      if options[:yaml]
        # If yaml passed, deduce code to save model, and run...

        txt = %`
          txt = #{txt.inspect}
          mods = YAML::load(txt)

          mods = [mods] if ! mods.is_a?(Array)
          mods.each do |mod|
            mod.instance_variable_set('@partial_writes', false)

            existing = mod.class.where :id=>mod.id
            mod.instance_variable_set('@new_record', true) if existing.empty?

            mod.save
          end
          `
      end

      # Tmp > Hard-coded
      file = File.expand_path "/Users/craig/Dropbox/xikihub_rails/tmp/rails_run_tmp.txt"

      FileUtils.mkdir_p File.dirname(file)   # Create dir if it doesn't exist
      File.open(file, "w") { |f| f << txt }

      # Temp > Hard-coded > !!!!!!!
      uri = URI("http://docker.loc/_xikidev")



      response = Net::HTTP.get_response(uri)

      return "| The rails server doesn't appear to be running.  Start default server?\n=rails/start/" if response == :exception

      if response.response.is_a?(Net::HTTPNotFound)

        return self.suggest_installing_dev_controller if response.body =~ /<h2>No route matches .+\/_xikidev/

        # Todo > probably restore suggesting directly implanting the controller
        # return self.suggest_installing_plugin(options) if response.body =~ /<h2>No route matches .+\/xikidev/


        return response.body[/<h2>(.+)/, 1].sub(/<\/h2>.*/, '').gsub("&#39;", "'")
      end

      # Yaml data launched, so just pretend like it was saved?...
      # What about displaying errors?

      return "<* - saved!" if options[:yaml]



      txt = response.body
      txt.gsub!(/ +$/, '')

      # If is error, delete html at top
      txt.sub!(/.+?<h1>/m, '<h1>') if txt =~ /<div id="traces">/ && txt =~ /<h1>/

      # If file not found, suggest generating it
      if txt =~ /uninitialized constant XikidevController::(\w+)/
        clazz = $1
        return "
          > Class '#{clazz}' doesn't exist.  Generate it as a model?
          =#{self.running_dir}/
            =rails/generate/model/#{clazz}
          "
      end

      txt
    end

    def self.running_dir
      raise "Todo > port HTTParty call to built-in ruby call"
      HTTParty.get("http://localhost:3000/xikidev/dir").body rescue :exception
    end

    def self.suggest_installing_plugin options
      # Duh, can't get it yet because can't talk to app that doesn't have controller
      dir = self.running_dir

      %`
      | The running rails app doesn't appear to have the Xiki plugin
      | installed.  This plugin lets Xiki evaluate code in it in dev mode.
      =/path/to/your/rails/app/
        - 1. Add this line:
        - Gemfile
          | gem 'xiki-rails'
        - 2. Run bundler:
        % bundle install
      `
    end


    def self.jump_to_last_error app_root
      app_root = Bookmarks[app_root]

      txt = File.read("#{app_root}log/development.log")

      errors = txt.split(/^[A-Za-z:]+(Error|MissingTemplate) /)
      last_error = errors[-1]
      file, line = last_error.match(/^  (\w.+?):(\d+)/)[1..2]

      View.open "#{app_root}#{file}"
      View.line = line
      nil

    end

    def self.suggest_installing_dev_controller
      %`
        > You don't have the xiki dev controller installed
        = ~/projects/your_rails_app/
          - 1. Create the controller:
          - app/controllers/
            - xikidev_controller.rb
              : class XikidevController < ApplicationController
              :   def index
              :
              :     file = File.expand_path "~/.xiki/misc/tmp/rails_run_tmp.txt"
              :
              :     ip = request.remote_ip
              :     return render(:text=>"Disabled unless development and called locally.") if ! Rails.env.development? || (ip != "127.0.0.1" && ip != "::1")
              :     return render(:text=>"The file where the caller should have put the code to eval doesn't exist: "+file) if ! File.exists? file
              :
              :     code = File.read file
              :     txt = instance_eval code, "/tmp/eval", 1
              :
              :     File.open(file, "w") { |f| f << "" }   # Remove code from temp location
              :     render plain: txt
              :
              :   end
              : end
          - 2. Add a route:
          - config/routes.rb
            : get '/_xikidev' => 'xikidev#index'
      `.unindent
    end


  end
end
