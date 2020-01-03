module Xiki
  module Menu
    class Bootstrap

      #   @@relative = ""
      #   @@relative = "http://xiki.loc"
      #   @@assets = "#{@@relative}/assets"
      #   @@assets = "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1"
      #   @@assets = "http://twitter.github.com/bootstrap/assets"

      def self.menu_after *args
        return if args[0]   # If menu outputted something, just return

        self.content *args[1..-1]
      end

      # Called when MENU set, so subclasses can use it to make
      # MENU have their name.
      def self.menu_constant
#        <= .make a page/
        txt = %`
        - examples/
          - layouts/
            - hello world/
              = bootstrap/
                > A heading
                You can just use a greater than sign for a heading.
            - columns/
              = bootstrap/
                > Hi
                Since these two sections are right next to each other, they'll show up side by side (as columns).
                > Snails
                Cool swirly shell but creepy antennae things.
            - hero unit/
              = bootstrap/
                - hero/
                  h1/Sharks
                  p/They have fins and other cool stuff.
                > Colors?
                Each time you generate the page, it uses a different background color for the hero, just to give you ideas.  Under "more control" there's an example of using a style to set it to a fixed color or gradient.
            - rows/
              = bootstrap/
                > Snails
                Cool swirly shell but creepy antennae things.
                > random
                This heading is random since we used the special heading "random".

                > Another
                This div is a underneath because of the blank line separating it.  If you removed the blank line, it would be a third column.
            - icons/
              = bootstrap/
                > $ Snails
                The "$" is replaced with a random icon.
                When you have "Lorem..." or "Ipsum Dolor..." or something like that, it uses sample text.
                > $ Sample text
                Lorem...
            - html/
              = bootstrap/
                > You can put html pretty much anywhere
                <pre>Here's me using <b>some</b> html.</pre>
                <ul><li>aww</li><li>yea-uh</li></ul>
            - more control/
              - 2 columns/
                = bootstrap/
                  - project name/Sharks
                  - row/
                    - span5/
                      > random
                      lorem ipsum...
                    - span7/
                      > random
                      ipsum dolor...
              - 3 columns/
                = bootstrap/
                  - project name/Sharks
                  - row/
                    - span3/
                      > random
                      lorem ipsum...
                    - span6/
                      > random
                      ipsum dolor...
                    - span3/
                      > random
                      dolor sit...
              - hero and icons/
                = bootstrap/
                  - project name/Sharks
                  - hero/
                    h1/Sharks
                    p/lorem...
                  - row/
                    - span6/
                      h2 icon/random
                      ipsum dolor sit...
                    - span6/
                      h2 icon/random
                      dolor sit lorem...
                  - row/
                    - span4/
                      h2 icon/random
                      lorem ipsum...
                    - span4/
                      h2 icon/random
                      ipsum dolor...
                    - span4/
                      h2 icon/random
                      sit lorem...
              - with styled hero/
                = bootstrap/
                  - hero/
                    h1/Sharks
                    p/lorem...
                  - h2/random
                  - p/sit lorem ipsum dolor...
                  - style/
                    | .hero-unit {
                    |   background-color: #9D261D;
                    |   color: #fff;
                    |   text-shadow: 2px 2px 2px #333;
                    |   border-radius: 20px;
                    | }
              - modified navbar/
                = bootstrap/
                  navbar/
                    | <img src="http://xiki.org/images/bootstrap_icon.png">
                    | <style>.navbar .container { padding: 1px 30px 0px; }</style>
                  h2/random
                  p/sit lorem ipsum dolor...
          - components/
            - buttons/
              = bootstrap/
                - h2/Buttons
                - p/
                  <a class="btn" href="">Hi</a>
                  <a class="btn btn-primary" href="">Hi</a>
                  <a class="btn btn-info" href="">Hi</a>
                  <a class="btn btn-success" href="">Hi</a>
                  <a class="btn btn-warning" href="">Hi</a>
                  <a class="btn btn-danger" href="">Hi</a>
                  <a class="btn btn-inverse" href="">Hi</a>
                - p/
                  <a class="btn btn-large" href="">Hi</a>
                  <a class="btn btn-large btn-success" href="">Hi</a>
                  <a class="btn btn-small" href="">Hi</a>
                  <a class="btn btn-mini" href="">Hi</a>
                  <a class="btn disabled" href="">Hi</a>
            - icons/
              - with headings/
                = bootstrap/
                  p/
                    - h3/random
                    - h2 icon/Info
                    - h2 icon/Info
                  p/
                    - h3/Specific
                    - h2/
                      - icon/043_group
                      Info
                    - h2/
                      - icon/051_eye_open
                      Info
              - all/
                - container/
                  - icon/
            - small icons/
              - container/
                - h2/Icons
                - p/
                  <i class="icon-user"></i>
                  <i class="icon-bullhorn"></i>
                  <i class="icon-glass"></i>
                  <i class="icon-music"></i>
                  <i class="icon-search"></i>
                  <i class="icon-envelope"></i>
                  <i class="icon-heart"></i>
                  <i class="icon-star"></i>
                - p/
                  <a class="btn" href=""><i class="icon-glass"></i> Glass</a>
                  <a class="btn btn-primary" href=""><i class="icon-search"></i> Search</a>
                  <a class="btn btn-danger" href=""><i class="icon-music"></i> Music</a>
            - forms/
              - basics/
                = bootstrap/
                  | <form class="well">
                  |   <label>Label name</label>
                  |   <input type="text" class="span3" placeholder="Type something?">
                  |   <span class="help-block">Example block-level help text here.</span>
                  |   <label class="checkbox">
                  |     <input type="checkbox"> Check me out
                  |   </label>
                  |   <button type="submit" class="btn">Submit</button>
                  | </form>
              - search/
                = bootstrap/
                  | <form class="well form-search">
                  |   <input type="text" class="input-medium search-query">
                  |   <button type="submit" class="btn">Search</button>
                  | </form>
              - inline/
                = bootstrap/
                  | <form class="well form-inline">
                  |   <input type="text" class="input-small" placeholder="Email">
                  |   <input type="password" class="input-small" placeholder="Password">
                  |   <label class="checkbox">
                  |     <input type="checkbox"> Remember me
                  |   </label>
                  |   <button type="submit" class="btn">Sign in</button>
                  | </form>
              - horizontal/
                = bootstrap/
                  | <form class="form-horizontal">
                  |   <fieldset>
                  |     <legend>Legend text</legend>
                  |     <div class="control-group">
                  |       <label class="control-label" for="input01">Text input</label>
                  |       <div class="controls">
                  |         <input type="text" class="input-xlarge" id="input01">
                  |         <p class="help-block">Supporting help text</p>
                  |       </div>
                  |     </div>
                  |   </fieldset>
                  | </form>
            - code/
              = bootstrap/
                - pre/
                  | class Clam
                  |   def hi
                  |     "hello"
                  |   end
                  | end
                - p/Hey <code>you</code> there.
            - carousel/
              = bootstrap/
                - <div id="myCarousel" class="carousel slide">
                  - <div class="carousel-inner">
                    <div class="active item">
                      dolor...
                    <div class="item">
                      ipsum...
                    <div class="item">
                      sit...
                  <a class="carousel-control left" href="#myCarousel" data-slide="prev">&lsaquo;</a>
                  <a class="carousel-control right" href="#myCarousel" data-slide="next">&rsaquo;</a>
                - script/
                  $('.carousel').carousel({
                  interval: 2000
                  })
        - docs/
          > Summary
          | Generate a good looking html page using Twitter's Bootstrap.

          > How to do it
          | Expand the 'container' menu to build, or start with the
          | 'example' menu.
          |
          | =bootstrap/
          |   h2/Example Heading

          > Bootstrap site
          =http://twitter.github.com/bootstrap/
            - javascript.html
            - base-css.html
            - getting-started.html#examples
        - see/
          =fontawesome/
        `
        name = self.name.to_s.downcase.sub(/.+:/, '')
        txt.gsub!("=bootstrap/", "=#{name}/") if name != "bootstrap"

        txt

      end

      MENU = self.menu_constant

      # @@bg = ['#eee', '#555', '#999', '#9D261D', '#369', '#096']


      # Temp >>> Hard-coded > because tomato color is the best!
      @@bg = ['#9D261D']


      @@tags = {
        "container"=>"div class='container'",
        "row"=>"div class='row'",
        "hero"=>"div class='hero-unit' id='hero'",
        "h2 icon"=>"h2",
      }

      @@filler = {
        "project name"=>"Welcome",
        "h2"=>"random",
        "h2 icon"=>"random",
        "p"=>"lorem ipsum...",
      }

      @@icons = [
        'expand-alt', 'meh', 'keyboard', 'microphone', 'microphone-off', 'shield', 'calendar-empty', 'css3', 'anchor', 'bullseye', 'check-minus', 'check-sign', 'edit-sign', 'adjust', 'asterisk', 'ban-circle', 'bar-chart', 'barcode', 'beaker', 'beer', 'bell-alt', 'bell', 'bolt', 'book', 'bookmark-empty', 'bookmark', 'briefcase', 'bullhorn', 'calendar', 'camera-retro', 'camera', 'certificate', 'check-empty', 'check', 'circle-blank', 'circle', 'cloud-download', 'cloud-upload', 'cloud', 'code-fork', 'code', 'coffee', 'cog', 'cogs', 'collapse-alt', 'comment-alt', 'comment', 'comments-alt', 'comments', 'credit-card', 'crop', 'dashboard', 'desktop', 'download-alt', 'download', 'edit', 'ellipsis-horizontal', 'ellipsis-vertical', 'envelope-alt', 'envelope', 'eraser', 'exchange', 'exclamation-sign', 'exclamation', 'external-link-sign', 'external-link', 'eye-close', 'eye-open', 'facetime-video', 'fighter-jet', 'film', 'filter', 'fire-extinguisher', 'fire', 'flag-alt', 'flag-checkered', 'flag', 'folder-close-alt', 'folder-close', 'folder-open-alt', 'folder-open', 'food', 'frown', 'gamepad', 'gift', 'glass', 'globe', 'group', 'hdd', 'headphones', 'heart-empty', 'heart', 'home', 'inbox', 'info-sign', 'info', 'key', 'laptop', 'leaf', 'legal', 'lemon', 'level-down', 'level-up', 'lightbulb', 'location-arrow', 'lock', 'magic', 'magnet', 'mail-forward', 'mail-reply', 'mail-reply-all', 'map-marker', 'minus-sign-alt', 'minus-sign', 'minus', 'mobile-phone', 'money', 'move', 'music', 'off', 'ok-circle', 'ok-sign', 'ok', 'pencil', 'phone-sign', 'phone', 'picture', 'plane', 'plus-sign', 'plus', 'print', 'pushpin', 'puzzle-piece', 'qrcode', 'question-sign', 'question', 'quote-left', 'quote-right', 'random', 'refresh', 'remove-circle', 'remove-sign', 'remove', 'reorder', 'reply-all', 'reply', 'resize-horizontal', 'resize-vertical', 'retweet', 'road', 'rocket', 'rotate-left', 'rotate-right', 'rss-sign', 'rss', 'screenshot', 'search', 'share-alt', 'share-sign', 'share', 'shopping-cart', 'sign-blank', 'signal', 'signin', 'signout', 'sitemap', 'smile', 'sort-down', 'sort-up', 'sort', 'spinner', 'star-empty', 'star-half-full', 'star-half-empty', 'star-half', 'star', 'tablet', 'tag', 'tags', 'tasks', 'terminal', 'thumbs-down', 'thumbs-up', 'ticket', 'time', 'tint', 'trash', 'trophy', 'truck', 'umbrella', 'unlock-alt', 'unlock', 'upload-alt', 'upload', 'user', 'volume-down', 'volume-off', 'volume-up', 'warning-sign', 'wrench', 'zoom-in', 'zoom-out', 'file', 'file-alt', 'cut', 'copy', 'paste', 'save', 'undo', 'repeat', 'text-height', 'text-width', 'align-left', 'align-center', 'align-right', 'align-justify', 'indent-left', 'indent-right', 'font', 'bold', 'italic', 'strikethrough', 'underline', 'superscript', 'subscript', 'link', 'unlink', 'paper-clip', 'columns', 'table', 'th-large', 'th', 'th-list', 'list', 'list-ol', 'list-ul', 'list-alt', 'angle-left', 'angle-right', 'angle-up', 'angle-down', 'arrow-down', 'arrow-left', 'arrow-right', 'arrow-up', 'caret-down', 'caret-left', 'caret-right', 'caret-up', 'chevron-down', 'chevron-left', 'chevron-right', 'chevron-up', 'chevron-sign-left', 'chevron-sign-right', 'chevron-sign-up', 'chevron-sign-down', 'circle-arrow-down', 'circle-arrow-left', 'circle-arrow-right', 'circle-arrow-up', 'double-angle-left', 'double-angle-right', 'double-angle-up', 'double-angle-down', 'hand-down', 'hand-left', 'hand-right', 'hand-up', 'play-circle', 'play-sign', 'play', 'pause', 'stop', 'eject', 'backward', 'forward', 'fast-backward', 'fast-forward', 'step-backward', 'step-forward', 'fullscreen', 'resize-full', 'resize-small', 'facebook', 'facebook-sign', 'twitter', 'twitter-sign', 'github', 'github-sign', 'html5', 'linkedin', 'linkedin-sign', 'maxcdn', 'pinterest', 'pinterest-sign', 'google-plus', 'google-plus-sign', 'ambulance', 'h-sign', 'hospital', 'medkit', 'plus-sign-alt', 'stethoscope', 'user-md',
      ]

      @@random = ["Info", "Facts", "B. S.", "Why?", "History", "Traits", "Story", "What?", "Happy"]
      @@random_index = 0

      # Render txt as html.
      # Bootstrap.render "h1/foo"
      def self.render txt

        txt.gsub! /^ *\|-.*\n/, ''   # Delete |-... "comments"

        bootstrap = Bootstrap.new txt
        bootstrap.to_html   #> |||
        bootstrap.wrap_html_page
      end

      def self.make_a_page
        siblings = Tree.siblings :cross_blank_lines=>1

        # If just 3 siblings, continue as usual

        # If more siblings
        "
        - hero/
        - row/
        - icon/
        - style/
        - project name/
        "
      end

      def self.simplified_render txt

        snippet = txt.unindent

        html = %`
          <!DOCTYPE html>
          <html lang="en">
            <head>
              <meta charset="utf-8">
              <meta http-equiv="X-UA-Compatible" content="IE=edge">
              <meta name="viewport" content="width=device-width, initial-scale=1">
              <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
              <meta name="description" content="">
              <meta name="author" content="">
              <link rel="icon" href="../../favicon.ico">

              <title>Bootstrap</title>

              <!-- Bootstrap core CSS -->
              <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">

              <!-- IE10 viewport hack for Surface/desktop Windows 8 bug -->
              <link href="../../assets/css/ie10-viewport-bug-workaround.css" rel="stylesheet">

              <!-- Custom styles for this template -->
              <link href="jumbotron.css" rel="stylesheet">

              <!-- Just for debugging purposes. Don't actually copy these 2 lines! -->
              <!--[if lt IE 9]><script src="../../assets/js/ie8-responsive-file-warning.js"></script><![endif]-->
              <script src="../../assets/js/ie-emulation-modes-warning.js"></script>

              <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
              <!--[if lt IE 9]>
                <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
                <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
              <![endif]-->


              <style>
                /* Move down content because we have a fixed navbar that is 50px tall */
                body {
                  padding-top: 50px;
                  padding-bottom: 20px;
                }
              </style>

            </head>

            <body>

              <nav class="navbar navbar-inverse navbar-fixed-top">
                <div class="container">
                  <div class="navbar-header">
                    <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                      <span class="sr-only">Toggle navigation</span>
                      <span class="icon-bar"></span>
                      <span class="icon-bar"></span>
                      <span class="icon-bar"></span>
                    </button>
                    <a class="navbar-brand" href="#">Project name</a>
                  </div>
                </div>
              </nav>

#{snippet}

              <!-- Bootstrap core JavaScript
              ================================================== -->
              <!-- Placed at the end of the document so the pages load faster -->
              <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
              <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>
              <!-- IE10 viewport hack for Surface/desktop Windows 8 bug -->
              <script src="../../assets/js/ie10-viewport-bug-workaround.js"></script>
            </body>
          </html>

        `


        return Browser.html html



      end


      def self.content *args
        prefix = Keys.prefix :clear=>1

        # If not expandable or open+, render in browser

        if prefix == "open" || Line !~ /\/$/ || Line =~ /^ *\|/
          return self.simplified_render args[0]

          Ol "Todo > clean up unused stuff below here!"

          txt = args[0]
          txt = txt.unindent
          txt = "<div class='container'>#{txt}</div>"

          txt = self.render txt   #> ||
          return Browser.html txt
        end

        last = args.last
        if filler = @@filler[last]
          Line.add_slash :txt=>filler, :no_move=>1
        elsif last == "icon"
          @@icons.map{|o| "- #{o}\n"}.join('')
        elsif last == 'style'
          "
          | .hero-unit {
          |   background-color: #9D261D;   /* #eee #555 #999 #9D261D #369 #096 */
          |   background: -moz-linear-gradient(top, #111, #444);
          |   background: -webkit-linear-gradient(top, #111, #444);
          |   color: #fff;
          |   text-shadow: 2px 2px 2px #333;
          |   border-radius: 20px;
          | }
          "
        elsif args == ['row']
          "
          - span4/
          - span6/
          - span12/
          "
        elsif args == ['hero']
          "
          - h1/random
          - p/lorem ipsum...
          "
        else
          "
          - h2/random
          - p/lorem ipsum...
          "
        end
      end

      def initialize txt
        @txt = txt
      end

      # If any >... lines at left margin, interpret them as h2's and assume
      # we want to auto-wrap in row/spanN/ items.
      #
      # If any >... lines indented, just wrap in h2.
      def self.expand_wiki_headings txt

        # Always simply wrap >... lines that are indented...
        txt.gsub! /^( *hero\/\n *)> /, "\\1h1/"   # If under "hero/" make h1

        txt.gsub! /^( +)> /, "\\1h2/"

        return if txt !~ /^> /   # If no >... lines at left margin, we're done (because only they have are meant to be wrapped)

        # Pre-process to deal with >... lines...

        row_i = 0
        row_hash = {0=>0}   # example: 0=>2, 1=>2
        lines = txt.split "\n"

        # Preprocess to see how many spans there will be for each row...

        no_h2_yet = true
        lines.each do |l|
          if l =~ /^> /   # If >..., increase count in hash
            row_hash[row_i] += 1
            no_h2_yet = false
          elsif no_h2_yet   # Leave items before first >... alone
            next
          elsif l =~ /^$/
            row_i += 1
            row_hash[row_i] = 0
          end
        end

        # Go through each, wrapping and adding h2/ and p/ ...

        txt.replace ""

        no_h2_yet = true
        row_i = 0
        lines.each do |l|
          if l =~ /^> (\$ )?(.+)/   # If >..., increase count in hash
            icon, heading = $1, $2
            width = 12 / row_hash[row_i]
            txt << "row/\n" if no_h2_yet   # If first unindented >..., add initial row
            no_h2_yet = false
            next txt.<< "  span#{width}/\n    h2 icon/#{heading}\n" if icon
            txt << "  span#{width}/\n    h2/#{heading}\n"
          elsif no_h2_yet   # Leave items before first >... alone
            txt << "#{l}\n"
            next
          elsif l =~ /^$/
            row_i += 1
            txt << "row/\n"
          else   # Content, so add paragraph
            txt << "    p/#{l}\n"
          end
        end

      end


      # Used internally by .render.  Turn text in @txt into html.
      def to_html

        # Pre-process the text

        project_name = @txt.slice!(/[ +-]*project name\/(.*)\n/)

        # Remove |#... comments
        @txt.gsub! /^ *\|# .+\n/, ''

        # Wrap "- container/" around all, unless already a container
        if @txt !~ /^ *([+-] )?container\// && @txt !~ /^ *([+-] )?<div.+class=.container\b/
          @txt.gsub!(/^/, '  ')
          @txt = "- container/\n#{@txt}"
        end

        @project_name = project_name ? project_name[/\/(.+)/, 1] : "Welcome"

        @html = @txt   # Convert to html

        # Expand out tags into actual html...

        # Expand out spanN tags.

        @html.gsub!(/<(\/?)span(\d+)>/) do |o|
          slash, number = $1, $2
          slash.any? ?
            "</div>" :
            "<div class='span#{number}'>"
        end

        # Add random icons in <h2 icon> tags.

        @html.gsub!("<h2 icon>") { |o| "<h2> #{Bootstrap.icon_tag @@icons[rand(93)]}" }

        # Make all lines within <icon> tags become icons.

        @html.gsub!(/( *)<icon>\n(.+?) *<\/icon>\n/m) do |o|
          indent, txt = $1, $2
          txt.gsub(/ *(.+)/) do |o|
            name = $1
            "#{indent}#{Bootstrap.icon_tag name}"
          end
        end

        # Pull out certain tags, so they can be used elsewhere.

        # Created by - navbar/ item if one was there
        @navbar = @html.slice!(/ *<navbar>\n(.+?) *<\/navbar>\n/m)

        @scripts = ""
        # If pull out multiple script tags
        @html.gsub!(/ *(<script>\n.+?<\/script>\n)/m) do
          @scripts << $1
          ""
        end

        # Expand out predictable fake html tags

        @html.gsub!(/<(.+?)>/) do |o|
          tag = $1
          slash = tag.slice! /^\//
          tag = @@tags[tag]
          next o if ! tag
          tag = tag.sub(/ .+/, '') if slash
          "<#{slash}#{tag}>"
        end

        @@random_index = rand(@@random.length)
        @html.gsub!(/^( *)random$/i) do
          indent = $1
          "#{indent}#{@@random[(@@random_index += 1) % @@random.length]}"
        end

        @html
      end

      def self.icon_tag name
        "<i class='icon icon-#{name}'></i>"
      end

      def wrap_html_page
        navbar = @navbar || "        <a class='brand' id='brand' href='#'>#{@project_name}</a>"

        result = %`
          <!DOCTYPE html>
          <html lang="en">
            <head>
              <meta charset="utf-8">
              <title>#{@project_name}</title>
              <meta name="viewport" content="width=device-width, initial-scale=1.0">

              <!-- Le styles -->
              <!-- <link href="../assets/css/bootstrap.css" rel="stylesheet"> -->
              <style type="text/css">
                body {
                  padding-top: 60px;
                  padding-bottom: 40px;
                }

              </style>
              <!-- <link href="../assets/css/bootstrap-responsive.css" rel="stylesheet"> -->

              <!--

                /* Temp Hack to get icons to not be fucked up. */
                .icon{
                  line-height: 38px !important;
                  width: 33px !important;
                }


              <link href="http://xiki.loc/font-awesome/3.1.1/css/font-awesome.min.css" rel="stylesheet">
              <link href="http://xiki.loc/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css" rel="stylesheet">
              <link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css" rel="stylesheet">
              -->

              <link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.no-icons.min.css" rel="stylesheet">
              <link href="http://netdna.bootstrapcdn.com/font-awesome/3.1.1/css/font-awesome.min.css" rel="stylesheet">



              <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
              <!--[if lt IE 9]>
                <script src="../assets/js/html5shiv.js"></script>
              <![endif]-->

              <!-- Fav and touch icons
              <link rel="apple-touch-icon-precomposed" sizes="144x144" href="../assets/ico/apple-touch-icon-144-precomposed.png">
              <link rel="apple-touch-icon-precomposed" sizes="114x114" href="../assets/ico/apple-touch-icon-114-precomposed.png">
                <link rel="apple-touch-icon-precomposed" sizes="72x72" href="../assets/ico/apple-touch-icon-72-precomposed.png">
                              <link rel="apple-touch-icon-precomposed" href="../assets/ico/apple-touch-icon-57-precomposed.png">
                                             <link rel="shortcut icon" href="../assets/ico/favicon.png">
              -->
            </head>

            <body>

              <div class="navbar navbar-inverse navbar-fixed-top">
                <div class="navbar-inner">
                  <div class="container">
                    #{navbar}
                    <!-- old navbar stuff moved from here to ruby comments below -->
                  </div>
                </div>
              </div>
          `.unindent


        if @html =~ /class='hero-unit'/
          bg = @@bg[rand @@bg.length]
          bg_is_light = bg =~ /^#[a-f][a-f][a-f]$/
          fg = bg_is_light ? '#000' : '#fff'   # Black if light color
          shadow = bg_is_light ? '#999' : '#333'   # Black if light color
          result += %`
            <style>
              .hero-unit {
                background-color: #{bg};
                color: #{fg};
                text-shadow: 2px 2px 2px #{shadow};
                border-radius: 20px;
              }


              /* Temp > Hard-coded!! */

              .hero-unit {
                padding: 40px;
              }
            </style>
            `.unindent.gsub(/^/, '    ')
        end

        result += @html.gsub(/^/, '    ')

        result += %`
              <!-- Le javascript
              ================================================== -->
              <!-- Placed at the end of the document so the pages load faster -->


              <!--
              <script src="http://xiki.loc/js/jquery.min.js"></script>
                -->
              <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>



              <script src="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/js/bootstrap.min.js"></script>

          #{@scripts}
            </body>
          </html>
          `.gsub(/^      /, '')

        result
      end

end; end; end
