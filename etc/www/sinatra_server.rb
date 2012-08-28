require 'sinatra'

set :port, 8161
set :bind, '127.0.0.1'

# TODO: Enable basic auth for security
  # Add menu to let users create a password
# use Rack::Auth::Basic, "Nope" do |u, p|
#   u == 'foo' && p == 'bar'
# end

get %r{/(.*)} do |path|
  index path
end

def htmlify txt, options={}

  return txt if request.env['HTTP_ACCEPT'] !~ /\Atext\/html/   # Only process if request wants html

  # If starts with <foo or @bootstrap, just render it as html or bootstrap...

  if txt =~ /\A<\w/
    return txt
  elsif txt =~ /\A@bootstrap\/\n  /

    xiki_dir = File.expand_path "#{File.dirname(__FILE__)}/../.."

    $:.unshift "#{xiki_dir}/lib"
    ["xiki/ol", "xiki/core_ext", "xiki/line", "xiki/tree", "../../menu/bootstrap"].each{|o| require o}

    txt.slice! /.+\n/
    txt = txt.unindent

    txt = Bootstrap.process txt

    return txt
  end

  # Turn menu text into html...

  # TODO: should probably maintain indenting?
  txt.gsub! /^ *\| ?/, ''

  txt.gsub! /^( *)[+-]+ /, "\\1"   # Get rid of bullets
  txt.gsub! /> (.+)/, "<h1>\\1</h1>"

  # Use relative links if path ends in slash, otherwise use absolute
  path = request.env['REQUEST_PATH']

  path = "#{path}/" if path !~ /\/$/

  # Make path be "/foo/bar/", if no slash ("/foo/bar")

  txt.gsub!(/^( *)(.+?)(\/?)$/){
    all = $&
    indent, item, slash = $1, $2, $3
    next all if all =~ /<h/ || all !~ /\/$/
    my_path = path
    my_path = "/" if item.slice!(/^<< /)
    my_path = "/" if item.slice!(/^@ ?/)
    "#{indent}<a href=\"#{my_path}#{item}\">#{item.gsub '_', ' '}</a>"
  } # no slash

  txt.gsub!(/^ +/){"&nbsp; " * $&.length}   # Get rid of bullets

  txt.gsub!(/.+/){
    all = $&
    next all if all =~ /<h/
    next "<p class='info'>#{all}</p>" if all !~ /^<a/   # Don't end in "/" - just informational
    all
  }
  txt.gsub! /^$/, "<p class='blank'>&nbsp;</p>"
  output = ""
  output <<  %`
  <style>
    a {
      text-decoration: none;
    }

    .content a.selected {
      background: -moz-linear-gradient(center top, #58b, #7ad) repeat scroll 0 0 transparent;
    }
    .content a {
      color: #666;
      display: block;

      border-radius: 2px;
      border: solid 1px;
      border-color: #eee #ddd #bbb #ddd;
      background: -moz-linear-gradient(center top, #fff, #f1f1f1) repeat scroll 0 0 transparent;
      background: -webkit-gradient(linear,center bottom,center top,from(#f1f1f1),to(#fff));
      display: block;
      padding: 10px 16px;
      text-shadow: 0 1px 0 #fff;
      font-weight: bold;
    }
    a:hover {
      background: -moz-linear-gradient(center top, #f8f8f8, #e1e1e1) repeat scroll 0 0 transparent;
      background: -webkit-gradient(linear,center bottom,center top,from(#e1e1e1),to(#f8f8f8));
    }
    body {
      font-family: arial;
      margin: 50px;
      font-size: 15px;
      color: #666;
      line-height: 21px;
      background-color: #f8f8f8;
    }
    p {
      margin: 0;
    }
    .blank {
      line-height: 12px;
    }
    h1 {
      color: #000;
      font-family: arial;
      font-size: 16px;
      font-weight: bold;
      margin: 11px 0 7px;
    }
    .save {
      margin: 0 20px 10px 20px;
      font-size: 15px;
    }
    form {
      margin: 0;
    }
    textarea {
      border: solid #ccc 1px;
      margin: 2px 20px 13px 20px;
      padding: 7px;
      width: 80%;
      font-size: 15px;
      color: #555;
      height: 150px;
      line-height: 20px;
    }
    .toggle {
      font-size: 13px;
      margin: 15px 20px 0px 20px;
      font-weight: bold;
    }
    .toggle a {
      color: #aaa;
    }
    .info {
      margin: 0 0 6px;
    }
    pre {
      font-weight: bold;
    }

  </style>
  `

  output <<  %`<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"></script>`

  if ! options[:no_keys]
    output <<  %`
    <script>
      filter_pattern = "";
      started_searching = false;
      $(function(){

        $(window).focus(function () {
          filter_pattern = "";
          started_searching = false;

          $('body').css("opacity", 1.0);
          $('p, h1, a').show();
          $('a').removeClass("selected");
        });

        $('body').keypress(function(e){
          if(e.ctrlKey || e.altKey || e.metaKey) return;
          var char = e.which;
          var letter = String.fromCharCode(char).toLowerCase();
          //console.log(char)
          if(char == 13){
            $("a:visible:eq(0)").click();
            e.stopPropagation();
            return false;
          }

          if(letter == " "){
            if(started_searching) filter_pattern = "";
            else window.location = "/";
            e.stopPropagation();
            return false;
          }

          if(! letter.match(/[0-9a-z]/)) return true;

          started_searching = true;
          filter_pattern += letter;

          //console.log(filter_pattern);

          // If more than 100, don't use fade
          var count = $('p, h1, a').filter(':visible').length;
          var fade = count < 50;

          $('p, h1, a').each(function(i, e){
            e = $(e);
            if(e.is(":hidden")) return;
            var html = e.html().replace(/<.+?>/g, '');
            //console.log(html);
            if(html.toLowerCase().indexOf(filter_pattern) == -1)
              fade ? e.slideUp(400) : e.hide();
            // Get content
              // Remove tags
                // Hide if remaining doesn't match regex (/on/)
          })

        });

        $('a').click(function(e){
          var target = $(e.target);
          target.toggleClass("selected");

          $('body').animate({opacity: 0.0}, {easing: 'swing', duration: 150});
          window.setTimeout(function(){ window.location = target.attr('href'); }, 150);
          return false;
        });

      })
    </script>
    `
  end

  "#{output}<div class='content'>#{txt}</div>"

end


def usage
  htmlify "
    > Summary
    This url displays xiki menus, which come from simple files found in ~/menu/.

    > Show all menus
    all/

    > Examples
    ip/
    animals/
    sharks/
    tables/

    > Create menu
    Just go to the url of a menu that doesn't exist yet:
    unicorn/

    > Docs
    docs/
    ".sub("\n", '').gsub(/^    /, '')
end


def index menu
  no_keys = false

  if ENV['REQUEST_METHOD'] == "POST"
    cgi = CGI.new
    post_txt = cgi['txt']

    # Temporary hack
    File.open("/tmp/post_tmp", "w") { |f| f << post_txt }

    # What's a better way to do this?
    # Pass in pipe, and send to shell command via STDIN

  end

  #     menu = ENV['QUERY_STRING']

  return usage if menu == ""

  # Run command...

  #     command = "/usr/local/bin/xiki #{menu}"
  command = "xiki #{menu}"
  txt = `#{command}`

  #   rescue e=>Exception
  #     puts "<pre>#{e.message}\n#{e.backtrace}</pre>"
  #     Ol << "If we get the permission problem, provide .notes file that will run command to run xiki command once (message explaining it first)!"
  #   end

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

  # Html-ify and print output...

  htmlify txt, :no_keys=>no_keys

rescue Exception=>e
  "<pre>#{e.message}\n#{e.backtrace}</pre>"
end

def camel_case txt
  return txt if txt !~ /_/ && txt =~ /[A-Z]+.*/
  txt.split('_').map{|e| e.capitalize}.join
end
