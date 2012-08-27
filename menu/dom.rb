class Dom

  def self.menu

    "
    - .styles/
    - docs/
      > Summary
      | Navigate and update the web page currently displayed in your
      | web browser.

      > Browse
      @dom/

      > Search text
      @dom/##Some text/

      > Id's and styles
      @dom/#my_id/
      @dom/.my_style/

      > Show all id's
      @dom/ids/

      > Show all styles
      @dom/styles/
    "
  end

  def self.menu_before *args
    return nil if args.any?

    # If no args, delegate to dom path

    result = self.dom *args
    result + ["docs/"]
  end

  def self.menu_after output, *args
    return nil if output

    # If menu couldn't handle it, delegate to dom path

    self.dom *args
  end

  def self.dom *args

    # If starts with ##, delegate to search...

    if args[0] =~ /^##(.+)/
      return self.search $1
    end

    options = args[-1].is_a?(Hash) ? args.pop : {}

    prefix = options[:prefix] || Keys.prefix

    raw_args = args.to_s.sub /\/$/, ''

    # If last arg has linebreakes, save back to page...

    save = args.pop if args.last =~ /^\|/

    args.each do |o|
      o.sub! /\/$/, ''
      o.sub!(/:(\d+)$/) { ":eq(#{$1.to_i - 1})" }
      o.sub!(/$/, ':eq(0)') if o !~ /[:#]/
    end
    args = ['html'] + args unless args[0] =~ /[.#]/
    args = args.join ' > '

    js = %`
      $.fn.blink = function(times) {
        console.log(times)

        var el = $(this);
        if(! times) times = 2;
        for(x=1;x<=times;x++) {
          el.animate({opacity: 0.0}, {easing: 'swing', duration: 200});
          el.animate({opacity: 1.0}, {easing: 'swing', duration: 200});
        }
        return this;
      };

      var kids = [];
      `.unindent

    if save
      save = Tree.siblings(:all=>true).map{|i| "#{i.gsub(/^\| /, '')}\n"}.join('')
      save.gsub! "\n", "\\n"
      save.gsub! '"', '\\"'

      js << %`
        var e = $(\"#{args}\");
        if(e.length)
          e.blink(1).html(\"#{save}\");
        `.unindent

      if args =~ /(.+) > (.+)/
        parent, child = $1, $2.sub!(/:.+/, '')
        js << %`
          else
            $(\"#{parent}\").blink(1).append(\"<#{child}>#{save}</#{child}>\");
          `.unindent
      end

      Firefox.exec js
      return
    end

    # Otherwise, just blink and show children...

    if prefix == :-
      Firefox.exec "$(\"#{args}\").blink()"
      return
    end
    if prefix != "all" && prefix != "outline"
      js << %`
        $("#{args}").blink().children().each(function(n, e){
          var tag = e.nodeName.toLowerCase();
          var id = $(e).attr('id');
          if(id) tag += "#"+id
          kids.push(tag);
        })
        `
    end

    js << %`
      if(kids.length)
        String(kids);
      else
        "html::"+$("#{args}").html();
      `.unindent

    kids = Firefox.exec js  #, :jquery=>1

    kids = kids.sub(/\A"/, '').sub(/"\z/, '') if kids =~ /\A"/
    if kids =~ /\Ahtml::/
      kids = kids.sub(/\Ahtml::/, '').strip
      return prefix == "all" ?
        kids.gsub(/^/, '| ') :
        self.tidy(kids, raw_args)
    end

    kids = kids.split ','

    counts = {}
    kids.each do |k|   # Add on counts
      k_raw = k.sub /#.+/, ''
      counts[k] ||= 0
      counts[k] += 1
      k.replace "#{k}:#{counts[k]}" unless k =~ /#/ || counts[k] == 1
    end
    kids.map{|o| "#{o}/"}
  end


  def self.search txt=nil
    if txt.nil?
      return View.prompt "Enter a string to search for on the page"
    end

    js = %`
      var result="", depth=-1;
      $('*:contains(#{txt})').each(function(i, e){
        var tag = e.tagName.toLowerCase();
        e = $(e);
        var current_depth = e.parents().length;   // Only climb downward
        if(current_depth <= depth) return depth = 9999;   // Don't grab any more
        depth = current_depth;
        var prev_count = $(e).prevAll(tag).length;
        result += tag;
        if(prev_count > 0) result += ':'+(prev_count+1);
        result += '/';
      })
      result;
      `.unindent

    result = Firefox.exec js  #, :jquery=>1

    result.sub "html/", "- @dom/"
  end


  def self.tidy html, tag=nil

    File.open("/tmp/tidy.html", "w") { |f| f << html }

    errors = `tidy --indent-spaces 2 --tidy-mark 0 --force-output 1 -i -wrap 0 -o /tmp/tidy.html.out /tmp/tidy.html`
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
    html.gsub! /^/, '| '
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

  #   def self.ids *args
  # Ol << "args: #{args.inspect}"

  #     # If just "ids/", list all

  #     if args.blank?
  #       js = %`
  #         var result="";
  #         $('*[id]').each(function(i, e){
  #           var id = $(e).attr('id');
  #           result += "+ #"+id+"/\\n";
  #         })
  #         result;
  #         `.unindent

  #       return Firefox.exec js
  #     end

  #     # Id passed so just navigate to it

  #     Dom.dom *args

  #     #     result.sub "html/", "- @dom/"

  #   end

  def self.styles
    "todo"
  end


end
