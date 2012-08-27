class Html
  def self.menu *args

    return Tree.<< Dom.dom(:prefix=>"all") if Keys.prefix == "all"
    return Tree.<< Dom.dom(:prefix=>"outline") if Keys.prefix == "outline"

    prefix = Keys.prefix :clear=>1

    # If as+open, or launched line without slash, render in browser...

    if prefix == "open" || (Line !~ /\/$/ && Line =~ /^ /)
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

    # Suggest relevant elements...

    last = args.last
    if filler = @@filler[last]
      Line.add_slash :txt=>filler, :left=>1
    elsif args == []
      ['html/', 'div/', 'style/']
    elsif last == 'html'
      ['head/', 'body/']
    elsif last == 'head'
      "- title/\n- style/\n| <script src='http://code.jquery.com/jquery.min.js'></script>"
    elsif last == 'style'
      "
      | body {
      |   font-family: arial;
      |   margin: 50px;
      | }
      "
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

end
