class Bootstrap

  @@assets = "http://twitter.github.com/bootstrap/assets"
  #   @@assets = "http://xiki.loc/assets"

  def self.menu_after *args
    return if args[0]   # If menu outputted something, just return

    self.content *args[1..-1]
  end

  def self.menu
    clazz = self.to_s.downcase

    %`
    <= .make a page/
    - examples/
      - layouts/
        - hello world/
          - @#{clazz}/
            - project name/Sharkathon
            - h2/Sharks
            - p/They have fins and other cool stuff.
        - hero unit/
          - @#{clazz}/
            - project name/Sharkathon
            - hero/
              - h1/Sharks
              - p/They have fins and other cool stuff.

            > Do you?
            Lorem Ipsum Dolor...
        - 2 columns/
          - @#{clazz}/
            - row/
              - span6/
                - h2/random
                - p/lorem ipsum...
              - span6/
                - h2/random
                - p/ipsum dolor...
        - 3 columns/
          - @#{clazz}/
            - row/
              - span4/
                - h2/random
                - p/lorem ipsum...
              - span4/
                - h2/random
                - p/ipsum dolor...
              - span4/
                - h2/random
                - p/dolor sit...
        - with icons/
          - @#{clazz}/
            - hero/
              - h1/Sharks
              - p/lorem...
            - row/
              - span6/
                - h2 icon/random
                - p/ipsum dolor sit...
              - span6/
                - h2 icon/random
                - p/dolor sit lorem...
            - row/
              - span4/
                - h2 icon/random
                - p/lorem ipsum...
              - span4/
                - h2 icon/random
                - p/ipsum dolor...
              - span4/
                - h2 icon/random
                - p/sit lorem...
        - shorthand/
          - @#{clazz}/
            - hero/
              > Shorthand
              p/Lines starting with ">" at left margin will have rows and spans auto-wrapped around them.

            > o random
            lorem...
            > o random
            lorem...

            > o random
            lorem...
            > o random
            lorem...
            > o random
            lorem...
        - with styled hero/
          - @#{clazz}/
            - hero/
              - h1/Sharks
              - p/lorem...
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
          - @#{clazz}/
            - navbar/
              | <img src="http://xiki.org/images/bootstrap_icon.png">
              | <style>.navbar .container { padding: 1px 30px 0px; }</style>
            - h2/random
            - p/sit lorem ipsum dolor...
      - components/
        - buttons/
          - @#{clazz}/
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
            - @#{clazz}/
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
            - @#{clazz}/
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
            - @#{clazz}/
              | <form class="well form-search">
              |   <input type="text" class="input-medium search-query">
              |   <button type="submit" class="btn">Search</button>
              | </form>
          - inline/
            - @#{clazz}/
              | <form class="well form-inline">
              |   <input type="text" class="input-small" placeholder="Email">
              |   <input type="password" class="input-small" placeholder="Password">
              |   <label class="checkbox">
              |     <input type="checkbox"> Remember me
              |   </label>
              |   <button type="submit" class="btn">Sign in</button>
              | </form>
          - horizontal/
            - @#{clazz}/
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
          - @#{clazz}/
            - pre/
              | class Clam
              |   def hi
              |     "hello"
              |   end
              | end
            - p/Hey <code>you</code> there.
        - carousel/
          - @#{clazz}/
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
      | @#{clazz}/
      |   h2/Example Heading

      > Bootstrap site
      @http://twitter.github.com/bootstrap/
        - javascript.html
        - base-css.html
        - getting-started.html#examples
    `
  end

  @@bg = ['#eee', '#555', '#999', '#9D261D', '#369', '#096']

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
    '000_glass', '001_leaf', '002_dog', '003_user', '004_girl', '005_car', '006_user_add', '007_user_remove', '008_film', '009_magic', '010_envelope', '011_camera', '012_heart', '013_beach_umbrella', '014_train', '015_print', '016_bin', '017_music', '018_note', '019_cogwheel', '020_home', '021_snowflake', '022_fire', '023_cogwheels', '024_parents', '025_binoculars', '026_road', '027_search', '028_cars', '029_notes_2', '030_pencil', '031_bus', '032_wifi_alt', '033_luggage', '034_old_man', '035_woman', '036_file', '037_credit', '038_airplane', '039_notes', '040_stats', '041_charts', '042_pie_chart', '043_group', '044_keys', '045_calendar', '046_router', '047_camera_small', '048_dislikes', '049_star', '050_link', '051_eye_open', '052_eye_close', '053_alarm', '054_clock', '055_stopwatch', '056_projector', '057_history', '058_truck', '059_cargo', '060_compass', '061_keynote', '062_attach', '063_power', '064_lightbulb', '065_tag', '066_tags', '067_cleaning', '068_ruller', '069_gift', '070_umbrella', '071_book', '072_bookmark', '073_signal', '074_cup', '075_stroller', '076_headphones', '077_headset', '078_warning_sign', '079_signal', '080_retweet', '081_refresh', '082_roundabout', '083_random', '084_heat', '085_repeat', '086_display', '087_log_book', '088_adress_book', '089_magnet', '090_table', '091_adjust', '092_tint', '093_crop', '094_vector_path_square', '095_vector_path_circle', '096_vector_path_polygon', '097_vector_path_line', '098_vector_path_curve', '099_vector_path_all',

    # Less interesting ones - don't show

    '100_font', '101_italic', '102_bold', '103_text_underline', '104_text_strike', '105_text_height', '106_text_width', '107_text_resize', '108_left_indent', '109_right_indent', '110_align_left', '111_align_center', '112_align_right', '113_justify', '114_list', '115_text_smaller', '116_text_bigger', '117_embed', '118_embed_close', '119_adjust', '120_message_full', '121_message_empty', '122_message_in', '123_message_out', '124_message_plus', '125_message_minus', '126_message_ban', '127_message_flag', '128_message_lock', '129_message_new', '130_inbox', '131_inbox_plus', '132_inbox_minus', '133_inbox_lock', '134_inbox_in', '135_inbox_out', '136_computer_locked', '137_computer_service', '138_computer_proces', '139_phone', '140_database_lock', '141_database_plus', '142_database_minus', '143_database_ban', '144_folder_open', '145_folder_plus', '146_folder_minus', '147_folder_lock', '148_folder_flag', '149_folder_new', '150_check', '151_edit', '152_new_window', '153_more_windows', '154_show_big_thumbnails', '155_show_thumbnails', '156_show_thumbnails_with_lines', '157_show_lines', '158_playlist', '159_picture', '160_imac', '161_macbook', '162_ipad', '163_iphone', '164_iphone_transfer', '165_iphone_exchange', '166_ipod', '167_ipod_shuffle', '168_ear_plugs', '169_albums', '170_step_backward', '171_fast_backward', '172_rewind', '173_play', '174_pause', '175_stop', '176_forward', '177_fast_forward', '178_step_forward', '179_eject', '180_facetime_video', '181_download_alt', '182_mute', '183_volume_down', '184_volume_up', '185_screenshot', '186_move', '187_more', '188_brightness_reduce', '189_brightness_increase', '190_circle_plus', '191_circle_minus', '192_circle_remove', '193_circle_ok', '194_circle_question_mark', '195_circle_info', '196_circle_exclamation_mark', '197_remove', '198_ok', '199_ban',


  ]

  @@random = ["Info", "Facts", "B. S.", "Why?", "History", "Traits", "Story", "What?", "Happy"]
  @@random_index = 0

  def self.make_a_page

    siblings = Tree.siblings :cross_blank_lines=>1

    # If just 3 siblings, contiune as usual

    # If more siblings
    CodeTree.do_kill_indented
    txt = ['project name/Foo', 'hero/', 'row/', 'style/'].map{|o| "- #{o}\n"}.join('')
    Line.previous
    Tree << txt

    nil   # Means we're handling collapse ourself
  end

  def self.content *args
    prefix = Keys.prefix :clear=>1

    # If not expandable or open+, render in browser

    if prefix == "open" || Line !~ /\/$/ || Line =~ /^ *\|/
      orig = Location.new

      Tree.to_root

      txt = Tree.children :cross_blank_lines=>1
      orig.go
      txt = txt.unindent

      txt = self.render txt

      return Browser.html txt
    end

    last = args.last
    if filler = @@filler[last]
      Line.add_slash :txt=>filler, :left=>1
    elsif last == "icon"
      @@icons
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
      ['span4/', 'span6/', 'span12/']
    elsif args == ['hero']
      ['h1/random', 'p/lorem ipsum...']
    else
      ['h2/random', 'p/lorem ipsum...']
    end

  end

  def initialize txt
    @txt = txt
  end

  #
  # If any >... lines at left margin, interpret them as h2's and assume
  # we want to auto-wrap in row/spanN/ items.
  #
  # If any >... lines indented, just wrap in h2.
  #
  def self.expand_wiki_headings txt

    # Always simply wrap >... lines that are indented...
    txt.gsub! /( +)> /, "\\1h2/"

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
      if l =~ /^> (\w )?(.+)/   # If >..., increase count in hash
        icon, heading = $1, $2
        width = 12 / row_hash[row_i]
        txt << "row/\n" if no_h2_yet   # If first unindented >..., add initial row
        no_h2_yet = false
        next txt.<< "  span#{width}/\n    h2 #{icon.strip}/#{heading}\n" if icon
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

  def to_html

    # Pre-process the text

    project_name = @txt.slice!(/[ +-]*project name\/(.*)\n/)

    # Remove |#... comments
    @txt.gsub! /^ *\|# .+\n/, ''

    Bootstrap.expand_wiki_headings @txt

    # Wrap "- container/" around all, unless already a container
    if @txt !~ /^ *([+-] )?container\// && @txt !~ /^ *([+-] )?<div.+class=.container\b/
      @txt.gsub!(/^/, '  ')
      @txt = "- container/\n#{@txt}"
    end

    @project_name = project_name ? project_name[/\/(.+)/, 1] : "Welcome"

    @html = Tree.to_html @txt   # Convert to html

    # Expand out tags into actual html...

    # Expand out spanN tags.

    @html.gsub!(/<(\/?)span(\d+)>/) do |o|
      slash, number = $1, $2
      slash.any? ?
        "</div>" :
        "<div class='span#{number}'>"
    end

    # Add random icons in <h2 icon> tags.

    @html.gsub!(/<h2 (\w+)>/) { |o| "<h2> #{Bootstrap.icon_tag @@icons[rand(93)]}" }

    # Make all lines within <icon> tags become icons.

    @html.gsub!(/( *)<icon>\n(.+?) *<\/icon>\n/m) do |o|
      indent, txt = $1, $2
      txt.gsub(/ *(.+)/) do |o|
        name = $1
        "#{indent}#{Bootstrap.icon_tag name}"
      end
    end

    # Pull out certain tags, so they can be used elsewhere.

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
    "<img class='bs-icon' src='http://xiki.loc/images/icons/glyphicons_#{name}.png'>"
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
          <link href="#{@@assets}/css/bootstrap.css" rel="stylesheet">
          <style>
            body {
              padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
              padding-bottom: 40px;
            }
          </style>
          <link href="#{@@assets}/css/bootstrap-responsive.css" rel="stylesheet">

          <!-- Le HTML5 shim, for IE6-8 support of HTML5 elements -->
          <!--[if lt IE 9]>
            <script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>
          <![endif]-->

          <!-- Le fav and touch icons -->
          <link rel="shortcut icon" href="#{@@assets}/ico/favicon.ico">
        </head>

        <body>

          <div class="navbar navbar-inverse navbar-fixed-top">
            <div class="navbar-inner">
              <div class="container">
        #{navbar}
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
        </style>
        `.unindent.gsub(/^/, '    ')
    end

    result += @html.gsub(/^/, '    ')

    result += %`
          <!-- Le javascript
          ================================================== -->
          <!-- Placed at the end of the document so the pages load faster -->
          <script src="#{@@assets}/js/jquery.js"></script>
          <script src="#{@@assets}/js/bootstrap.js"></script>

      #{@scripts}

        </body>
      </html>
      `.gsub(/^      /, '')

    result
  end

  def self.render txt
    bootstrap = Bootstrap.new txt
    bootstrap.to_html
    bootstrap.wrap_html_page
  end

end
