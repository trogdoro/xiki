class Html

  MENU_HIDDEN = "
    .outline/
    "

  def self.menu_before *args

    # If enter+all on blank menu, grab html from browser

    if args.blank? && Keys.prefix == "all"
      txt = Dom.dom(:prefix=>"all")
      return txt
    end

    nil
  end

  MENU = "
    | <h1>Sample</h1>
    | <p>Type some html here</p>
    + docs/
      > Summary
      | Put some html under this menu, and open it to render
      | it in the browser
    "
  # TODO: put this back, but under docs?
  #     <= outline/

  def self.outline
    "
    + html/
    + div/
    + style/
    "
  end

  def self.menu_after output, *args
    return if output
    return Tree.<< Dom.dom(:prefix=>"outline") if Keys.prefix == "outline"

    prefix = Keys.prefix :clear=>1

    # If as+open, or launched line without slash, render in browser...

    line = Line.value

    # If quote at left margin, just grab this paragraph

    if line =~ /^  \|/
      html = Tree.txt
      html = "#{Html.default_css}\n#{html}"
      return Browser.html html
    end

    # If single line html/foo, just run line in browser...

    if line =~ /^html\// && line !~ /\/$/
      txt = line[/\/(.+)/, 1]
      return Browser.html txt
    end

    # If as+open or line that shouldn't be auto-completed...

    if prefix == "open" || (line !~ /\/$/ && line =~ /^ /)
      orig = Location.new
      Tree.to_root
      txt = Tree.children :string=>1, :cross_blank_lines=>1
      orig.go
      txt = txt.unindent

      # Convert from tree to html if any ident, or 1st line doesn't start with "|"
      txt = txt =~ /^ / || txt !~ /\A\|/ ?
        Tree.to_html(txt) :
        txt.gsub(/^\| ?/, '')

      return Browser.html txt
    end

    # Insert stuff under if we recognize as auto-complete...

    last = args.last
    if filler = @@filler[last]
      Line.add_slash :txt=>filler, :left=>1
    elsif args == ['outline']
      ['html/', 'div/', 'style/']
    elsif last == 'html'
      ['head/', 'body/']
    elsif last == 'head'
      "- title/\n- style/\n| <script src='http://xiki.loc/assets/js/jquery.js'></script>"
    elsif last == 'style'
      Tree.quote Html.default_css :no_tags=>1
    elsif last == 'body'
      ['div/']
    else
      ['h1/Info', 'p/lorem ipsum...']
    end

  end

  def initialize txt
    @txt = txt
  end

  @@filler = {
    "h1"=>"Info",
    "title"=>"Welcome",
    "p"=>"Lorem ipsum...",
  }

  # Html.default_css
  # Html.default_css :no_tags=>1
  def self.default_css options={}
    txt = ""
    txt.<< "<style type='text/css'>\n" if ! options[:no_tags]
    txt.<< "
      body {
        font-family: arial;
        font-size: 13px;
        margin: 30px;
      }
      pre {
        background-color: #F8F8F8;
        border: 1px solid #CCCCCC;
        border-radius: 3px 3px 3px 3px;
        font-size: 13px;
        line-height: 19px;
        overflow: auto;
        padding: 6px 10px;
      }
    ".unindent
    txt.<< "</style>\n" if ! options[:no_tags]
    txt
  end

end
