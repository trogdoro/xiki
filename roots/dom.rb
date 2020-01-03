module Xiki
  class Dom

    def self.menu *args

      # MENU string couldn't handle it, so delegate to dom path...

      task = yield[:task]
      return "* html\n* tidy\n* element\n* attributes\n* all divs" if task == []

      options = {}

      options[:tidy_html] = 1 if task && task == ["tidy"]
      options[:element_html] = 1 if task && task == ["element"]
      options[:attributes] = 1 if task && task == ["attributes"]

      options[:html] = 1 if task && ["html", "all divs"].member?(task[0])

      txt = self.dom *args, options

      if task == ["all divs"]
        txt = txt.split("\n").grep(/<div/).join("\n")
      end

      txt
    end

    def self.dom *args
      # If starts with ##, delegate to search...

      if args[0] =~ /^##(.+)/
        return self.search $1
      end

      options = args[-1].is_a?(Hash) ? args.pop : {}

      prefix = options[:prefix] || Keys.prefix

      raw_args = args.to_s.sub /\/$/, ''

      save = args.pop if args.last =~ /\n/

      args.each do |o|
        o.sub! /\/$/, ''
        o.sub!(/:(\d+)$/) { ":eq(#{$1.to_i - 1})" }
        o.sub!(/$/, ':eq(0)') if o !~ /[:#]/
      end
      args = ['html'] + args unless args[0] =~ /^[.#*]/
      args[0].sub! /^\*/, ''   # *foo means just don't prepend "body >"
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

      # dom/foo/, so blink and show children...

      if options[:html]
        result = Tree.pipe Browser.js("return $(\"#{args}\").html()")
        return result
      elsif options[:tidy_html]
        result = Browser.js("return $(\"#{args}\").html()")
        result = Html.tidy result
        return Tree.pipe result
      elsif options[:element_html]
        result = Browser.js("return $(\"#{args}\")[0].outerHTML")
        result = Html.tidy result
        return Tree.pipe result
      elsif options[:attributes]
        result = Browser.js("return $(\"#{args}\")[0].outerHTML")
        result = Html.tidy result, :wrap_attributes=>1
        return Tree.pipe result
      end

      if ! save

        # Otherwise, just blink and show children...

        if prefix == :-
          Browser.js "$(\"#{args}\").blink()"
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
          if(kids.length){
            // return "yes";
            return String(kids);
          }else{
            // return "no";
            return "html::"+$("#{args}").html();
          }
          `.unindent

        kids = Browser.js js  #, :jquery=>1

        kids = kids.sub(/\A"/, '').sub(/"\z/, '') if kids =~ /\A"/
        if kids =~ /\Ahtml::/
          kids = kids.sub(/\Ahtml::/, '').strip
          return prefix == "all" ?
            Tree.quote(kids) :
            Html.tidy(kids, :tag=>raw_args).snippet
        end

        kids = kids.split ','

        counts = {}
        kids.each do |k|   # Add on counts
          k_raw = k.sub /#.+/, ''
          counts[k] ||= 0
          counts[k] += 1
          k.replace "#{k}:#{counts[k]}" unless k =~ /#/ || counts[k] == 1
        end
        return kids.map{|o| "- #{o}/"}.join("\n")
      end






      # dom/foo/| quoted, so save to page...

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

      Browser.js js
      Effects.glow :fade_in=>1

      ""

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
        return result;
        `.unindent

      result = Browser.js js  #, :jquery=>1

      result.sub "html/", "= dom/"
    end

  end
end
