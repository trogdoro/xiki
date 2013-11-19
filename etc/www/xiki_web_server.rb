require "sinatra/base"
require "cgi"
require "json"
require 'open3'

xiki_directory = `xiki dir`.strip

$:.unshift "#{xiki_directory}lib"
require 'xiki/core/ol.rb'
require 'xiki/core/files.rb'
require 'xiki/core/core_ext.rb'
require 'xiki/core/html.rb'

# TODO: Enable basic auth for security
  # Add menu to let users create a password
# use Rack::Auth::Basic, "Nope" do |u, p|
#   u == 'foo' && p == 'bar'
# end

class XikiWebServer < Sinatra::Base

  set :port, 8161
  set :bind, '127.0.0.1'

  get %r{^/_reload$} do
    xiki_directory = File.expand_path "#{File.dirname(__FILE__)}/../.."
    load "#{xiki_directory}/etc/www/xiki_web_server.rb"
    "@flash/- done"
  end

  get '/*' do
    index env['REQUEST_PATH'].sub(/^\//, '')
  end

  post '/*' do
    index env['REQUEST_PATH'].sub(/^\//, '')
  end

  # Handles /
  def default
    txt = call_command "web/docs", env
  end

  # Handles most requests
  def index menu

    # Probably only do this if it's requesting an image!
    # For now, if ends in .jpg, just assume they're requesting an image?
    # What if it's an actual menu item?  How to distinguish?
    menu.gsub! /\.jpg$/, ''

    no_keys = false

    return default if menu == ""

    # Run command...

    menu.sub! /^@/, ''
    menu.gsub! /-/, ' '

    txt = call_command menu, env

    # TODO: return different string when the service is down

    if txt.empty?
      menu = menu.sub /\/$/, ''

      no_keys = true

      if menu =~ /\/./   # If slash that's not at end
        puts "<pre>Nothing returned.  Maybe service is down, or maybe menu\njust returned nothing, run xiki command\n\n  $ xiki\n"
      else
        puts %`
          <h1>Menu '#{menu}' doesn't exist yet.  Create it?</h1>

          <form action="/create/#{menu}" method="post" id="as_menu">
          <div class='toggle'>
            <span>as text</span>
            | <a href="#" onclick="$('#as_class, #as_menu').toggle(); return false;">as class</a>
          </div>
          <textarea name="txt">
          - Sample item/
            - Another sample item/
          - Yet another/
          </textarea>
          <br>
          <input type="submit" value="save" class="save">
          </form>

          <form action="/create/#{menu}" method="post" id="as_class" style="display:none;">
          <div class='toggle'>
            <a href="#" onclick="$('#as_class, #as_menu').toggle(); return false;">as text</a>
            | <span>as class</span>
          </div>
          <textarea name="txt">
          class #{camel_case menu}
            def self.menu *args
              "Menu was called, with params \#{args.inspect}"
            end
          end
          </textarea>
          <br>
          <input type="submit" value="save" class="save">
          </form>

          `.gsub(/^        /, '')
      end
    end

    # if a file, read it manually...
    if txt =~ /^@file\/(.+)/
      file = $1
      content_type = {".jpg"=>"image/jpeg"}[File.extname file]
      content_type content_type
      return File.read(file, *Xiki::Files.encoding_binary)
    end

    txt

  rescue Exception=>e
    "<pre>#{e.message}\n#{e.backtrace}</pre>"
  end


  def call_command path, env

    # Only process as html if browser and not ajax...

    client = ""

    is_browser = env['HTTP_USER_AGENT'] =~ /\AMozilla/ ||
      env['HTTP_ACCEPT'] =~ /\Atext\/html/

    client = " -web" if is_browser && env["HTTP_X_REQUESTED_WITH"] != "XMLHttpRequest"

    # If POST, pipe params and path to command...

    if env['REQUEST_METHOD'] == "POST"
      options = "@options/#{JSON[params]}"
      return self.class.shell_command "xiki#{client} -", :stdin=>"#{options}\n#{path}"
    end

    # Otherwise, run command normally...

    command = "xiki#{client} '#{path}'"
    `#{command}`

  end


  def self.shell_command command, options={}

    #     stdin, stdout, stderr = Open3.popen3("#{profile}cd \"#{dir}\";#{command}")
    stdin, stdout, stderr = Open3.popen3(command)

    if txt = options[:stdin]
      stdin.puts txt
      stdin.close
    end

    result = ""
    result << stdout.readlines.join('')
    result << stderr.readlines.join('')

    result.force_encoding("binary") if result.respond_to? :force_encoding
    result.gsub!("\c@", '.')   # Replace out characters that el4r can't handle
    result
  end


  def camel_case txt
    return txt if txt !~ /_/ && txt =~ /[A-Z]+.*/
    txt.split('_').map{|e| e.capitalize}.join
  end
end

XikiWebServer.run!
