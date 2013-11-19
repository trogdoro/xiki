require "#{Xiki.dir}menu/dom.rb"  # if ! defined?(Xiki::Dom)

module Menu
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

    MENU = %`
      | <h1>Sample</h1>
      | <p>Type some html here</p>
      - examples/
        - forms/
          - text input/
            | <input type="text" name="foo">
          - textarea/
            | <textarea name="foo">This is the text</textarea>
          - select/
            | <select name="foo">
            |   <option value="volvo">Volvo</option>
            |   <option value="saab">Saab</option>
            | </select>
            - with groups/
              | <select>
              |   <optgroup label="German Cars">
              |     <option value="mercedes">Mercedes</option>
              |     <option value="audi">Audi</option>
              |   </optgroup>
              |   <optgroup label="Japanese Cars">
              |     <option value="toyota">Toyota</option>
              |     <option value="nissan">Nissan</option>
              |   </optgroup>
              | </select>
      - docs/
        - summary/
          | Put some html under this menu, and open it to render
          | it in the browser
        - outline/
          | You can use the hidden 'outline' item to quickly
          | build basic html outlines:
          <@ html/outline/
      `

    # TODO: put this back, but under docs?
    #     <= outline/

    def self.outline *args

      options = yield

      # /outline/|.../, so render outline...

      if options[:prefix] == "open" || Line =~ /[^\/]$/
        orig = Location.new
        Tree.to_root

        txt = Tree.children :string=>1, :cross_blank_lines=>1

        orig.go
        txt = txt.unindent

        # Convert from tree to html if any ident, or 1st line doesn't start with "|"
        txt = txt =~ /^ / || txt !~ /\A\|/ ?
          Xiki::Html.to_html_tags(txt) :
          txt.gsub(/^\| ?/, '')

        return Browser.html txt
      end

      # /outline/.../, so show items...

      last = args.last

      if filler = @@filler[last]
        Line.add_slash :txt=>filler, :left=>1
      elsif last == nil
        ['html/', 'div/', 'ul/', 'style/']
      elsif last == 'html'
        ['head/', 'body/']
      elsif last == 'ul'
        ['li/', 'li/']
      elsif last == 'head'
        "- title/\n- style/\n| <script src='http://xiki.loc/assets/js/jquery.js'></script>"
      elsif last == 'style'
        Tree.quote Xiki::Html.default_css :no_tags=>1
      elsif last == 'body'
        ['div/', 'ul/']
      elsif last == 'div'
        ['h1/Info', 'p/lorem ipsum...']
      else
        "| Hello"
      end

    end

    def self.menu_after output, *args
      return if output
      return Tree.<< Dom.dom(:prefix=>"outline") if Keys.prefix == "outline"

      options = yield

      prefix = Keys.prefix :clear=>1

      # If as+open, or launched line without slash, render in browser...

      # If quote at left margin, just grab this paragraph


      return "@beg/quoted/" if args[-1] =~ /^ *\|/ && args[-1] !~ /\n/

      if args[-1] =~ /\n/
        html = "#{Xiki::Html.default_css}\n#{args[-1]}"
        return Browser.html html
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
end
