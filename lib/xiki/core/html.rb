module Xiki
  class Html

    @@lorem = {
      "lorem"=>"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
      "ipsum"=>"Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
      "dolor"=>"Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
      "sit"=>"Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
    }

    # Turns a xiki tree into html, with a mobile_like interface.
    # Better name?
    #     def self.to_html_mobile txt
    #     def self.to_mobile txt
    def self.to_html txt, options={}

      # To translate items into html...

      txt = self.format_items txt, options if ! options[:dont_html_format_items]

      # To wrap surrounding html and styles etc....

      txt = self.template txt, options if ! options[:dont_html_template]

      txt
    end

    # Translate items into html (headings and mobile-like divs, etc.)
    def self.format_items txt, options={}

      if txt =~ /\A<\w/
        return txt
      elsif txt =~ /\A@html\/\n  /
        xiki_directory = File.expand_path "#{File.dirname(__FILE__)}/../../.."

        $:.unshift "#{xiki_directory}/lib"
        ["xiki/core/tree"].each{|o| require o}

        txt.slice! /.+\n/
        txt = txt.unindent
        txt = Tree.unquote txt

        return txt

      elsif txt =~ /\A@bootstrap\/\n  /

        xiki_directory = File.expand_path "#{File.dirname(__FILE__)}/../../.."

        ["xiki/core/ol", "xiki/core/core_ext", "xiki/core/line", "xiki/core/tree"].each{|o| require o}
        require "#{xiki_directory}/menu/bootstrap.rb"

        txt.slice! /.+\n/
        txt = txt.unindent

        txt = Xiki::Menu::Bootstrap.render txt

        options[:dont_html_template] = 1

        return txt
      end

      # Turn menu text into html...

      # TODO: should probably maintain indenting?
      txt.gsub! /^ *\| ?/, ''

      txt.gsub! /^( *)[+-]+ /, "\\1"   # Get rid of bullets
      txt.gsub! /^> (.+)/, "<h1>\\1</h1>"

      path = "/#{options[:path].gsub ' ', '-'}"

      path = "#{path}/" if path !~ /\/$/

      # Make path be "/foo/bar/", if no slash ("/foo/bar")

      txt.gsub!(/^( *)(.+?)(\/?)$/){
        all = $&
        indent, item, slash = $1, $2, $3
        next all if all =~ /<h/ || all !~ /\/$/
        my_path = path
        my_path = "/" if item.slice!(/^<< /)
        my_path = "/" if item.slice!(/^@ ?/)
        "#{indent}<a href=\"#{my_path}#{item.gsub ' ', '-'}\">#{item}</a>"
      } # no slash

      txt.gsub!(/^ +/){"&nbsp; " * $&.length}   # Get rid of bullets

      txt.gsub!(/.+/){
        all = $&
        next all if all =~ /<h/
        next "<p class='info'>#{all}</p>" if all !~ /^<a/   # Don't end in "/" - just informational
        all
      }
      txt.gsub /^$/, "<p class='blank'>&nbsp;</p>"
    end

    # Wrap surrounding html - style and js etc.
    def self.template txt, options={}

      name = options[:name] ? "Xiki - #{options[:name]}" : "Xiki"

      output = %`
      <head>
        <title>#{name}</title>
        <meta name="viewport" content="initial-scale = 1.0,maximum-scale = 1.0" />
      </head>
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
          font-family: dotsies;
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
          text-shadow: 1px 1px 1px #999;
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
          margin: 5px 0 4px;
        }
        pre {
          font-weight: bold;
        }

      </style>
      `

      output <<  %`<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>`
      #     output <<  %`<script src="http://xiki.loc/js/jquery.min.js"></script>`

      if ! options[:dont_html_format_items]
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



    # Turns a xiki tree with items like tag names into corresponding html tags.
    #
    # Html.to_html_tags "p/\n  hi\n"
    #   "<p>\n  hi\n</p>\n"
    def self.to_html_tags txt
      html = ""

      txt = txt.gsub /^( *)([+-] )?(\w[\w ]*\/)(.+)/, "\\1\\3\n\\1  \\4"   # Preprocess to break foo/Bar into separate lines

      previous = []

      Tree.traverse(txt) do |l, path|

        last = l.last
        next if !last   # Blank lines

        self.add_closing_tags html, l, previous   # If lower than last, add any closing tags

        last = Line.without_label :line=>last
        if last =~ /([^*\n]+)\/$/
          tag = $1
          html.<<("  " * (l.length-1)) unless l[-2] =~ /[ +-]*pre\/$/

          next html << "<#{tag}>\n"
        end

        last.sub! /^\| ?/, ''

        if last =~ /\.\.\.$/   # If "Lorem..." or "Lorem ipsum..." etc. make progressively longer
          old_length = last.length
          last.gsub!(/\w+/){|o| @@lorem[o.downcase] || o}
          last.sub!(/\.\.\.$/, '') if last.length != old_length   # Remove ... if we replaced something
        end

        parent = l[-2]
        html.<<("  " * (l.length-1)) unless parent =~ /[ +-]*pre\/$/

        html << "#{last}\n"
      end


      self.add_closing_tags html, [], previous

      html
    end

    def self.add_closing_tags html, l, previous

      if l.length <= previous.length
        left = l.length-1
        left = 0 if left < 0
        close_these = previous[left..-1]
        close_these.reverse.each_with_index do |tag, i|

          next if ! tag   # Was throwing error for icon/... in @bootstrap when hero exists

          tag.sub! /^\| ?/, ''
          tag = Line.without_label :line=>tag
          next if tag !~ /(.*\w)\/$/ && tag !~ /^<([^<\n]*[\w"'])>$/
          tag = $1
          tag = tag.sub(/ \w+=.+/, '')
          next if ["img"].member? tag
          html << "  " * (previous.length - i - 1)
          html << "</#{tag}>\n"
        end
      end
      previous.replace l
    end

    # Html.default_css
    # Html.default_css :no_tags=>1
    def self.default_css options={}
      txt = ""
      txt.<< "<style type='text/css'>\n" if ! options[:no_tags]
      txt.<< "
        body {
          font-family: arial;
          font-size: 14px;
          margin: 30px;
        }
        pre {
          background-color: #F8F8F8; border: 1px solid #CCCCCC; border-radius: 3px;
          font-size: 14px; line-height: 19px; overflow: auto; padding: 6px 10px;
        }
      ".unindent
      txt.<< "</style>\n" if ! options[:no_tags]
      txt
    end

  end
end
