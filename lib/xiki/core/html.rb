module Xiki
  class Html

    def self.to_html_tags txt

      html = ""

      txt = txt.gsub /^( *)([+-] )?(\w[\w ]*\/)(.+)/, "\\1\\3\n\\1  \\4"   # Preprocess to break foo/Bar into separate lines

      previous = []

      Tree.traverse(txt) do |l, path|

        last = l.last
        next if !last   # Blank lines

        self.add_closing_tags html, l, previous   # If lower than last, add any closing tags

        last = Line.without_label :line=>last
        if last !~ /^ *\| / && last =~ /([^*\n]+)\/$/
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

          next if tag =~ /^ *\|.+\/$/   # Skip if it's a quoted foo/ line

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



    def self.default_css options={}
      txt = ""
      txt.<< "<style type='text/css'>\n" if ! options[:no_tags]
      txt.<< "
        body {
          font-family: arial;
          font-size: 18px;
          margin: 30px;
        }
        pre {
          background-color: #F8F8F8; border: 1px solid #CCCCCC; border-radius: 3px;
          font-size: 14px; overflow: auto; padding: 6px 10px;
        }
      ".unindent

      txt.<< "</style>\n" if ! options[:no_tags]
      txt
    end

    # Turns a xiki tree with items like tag names into corresponding html tags.
    #
    # Html.to_html_tags "p/\n  hi\n"
    #   "<p>\n  hi\n</p>\n"
    # Html.default_css
    # Html.default_css :no_tags=>1

    # def self.tidy html, tag=nil
    def self.tidy html, options={}

      tag = options[:tag]


      File.open("/tmp/tidy.html", "w") { |f| f << html }

      more_options = "--indent-attributes y" if options[:wrap_attributes]

      result = `tidy #{more_options} y --new-blocklevel-tags a --wrap-attributes y --output-html y --indent auto --indent-spaces 2 --tidy-mark 0 --force-output 1 -i --wrap 0 -o /tmp/tidy.html.out /tmp/tidy.html`
      html = IO.read("/tmp/tidy.html.out")

      html.gsub! /\n\n+/, "\n"

      if tag == ""   # It's the whole thing, do nothing
      elsif tag == "head"
        html.sub! /.+?<head>\n/m, ''
        html.sub! /^<\/head>\n.+/m, ''
      else
        html.sub! /.+?<body>\n/m, ''
        html.sub! /^<\/body>\n.+/m, ''
      end

      html.gsub!(/ +$/, '')
      html.gsub! /^  /, '' unless html =~ /^</

      # Search and replace <a> and <span> elements onto separate lines
      html.gsub!(/( *).+> *<.+/) do |line|
        indent = $1
        line.gsub! /> *</, ">\n#{indent}<"
      end

      html

        # Try Nokogiri and xsl - Fucking dies part-way through
        #       return Nokogiri::XML(kids).human.sub(/\A<\?.+\n\n/, '').gsub(/^/, '| ')
        #       return Nokogiri::XML("<foo>#{kids}</foo>").human.gsub(/^/, '| ')
        #       return Nokogiri::XML(kids).human.gsub(/^/, '| ')

        # Try REXML (gives errors)
        #       doc = REXML::Document.new("<foo>#{kids}</foo>")
        #       out = StringIO.new; doc.write( out, 2 )
        #       return out.string

    end



  end
end

