class D3

  # v3 makes this not show the movement
  # @d3/examples/movement/position/
  # We're at .v2 now - go back to .v3 when this is fixed?
  # <script type="text/javascript" src="http://d3js.org/d3.v2.min.js"></script>
  # <script type="text/javascript" src="http://d3js.org/d3.v3.min.js"></script>

  MENU = %`
    | // Sample d3 code to modify and run
    | svg.append("rect").attr("x", 40).attr("y", 40).attr("width", 60).attr("height", 60);
    - docs/
      - summary/
        | This menu helps you use the d3 library, which renders
        | awesome graphs and charts etc. in your browser.
      - getting started/
        | Expand the root "svg" menu and check out the one-line
        | example.  Try modifying the numbers and re-running it.
        | Then look through the "examples" menu and do the same.
      - urls/
        - reference/
          @https://github.com/mbostock/d3/wiki/API-Reference
        - tutorials/
          @https://github.com/mbostock/d3/wiki/Tutorials
        - cool examples/
          @http://blog.visual.ly/creating-animations-and-transitions-with-d3-js/
    - see/
      << @d3/
    `

  def self.render txt

    html = ""
    #     html

    # replace/
    #   |-http://d3js.org/d3.v2.min.js
    #   |+http://d3js.org/d3.v2.min.js
    if txt !~ /<script/   # Wrap html if not there yet

      html << %`
        <script src="https://d3js.org/d3.v3.min.js" charset="utf-8"></script>
        <script src="http://code.jquery.com/jquery-2.0.0.js"></script>
        `.unindent
    end

    # If html...

    if txt =~ /\A<svg/
      html << txt+%`
        <script>
        var svg = d3.select("svg");
        </script>`.unindent
    else

    # If javascript...

      html << %`
        <div id="svg"></div>
        <script>
        var svg = d3.select("#svg").append("svg").attr("width", 800).attr("height", 800);
        `.unindent+txt+"</script>"
    end

    #     if html = options[:html]
    #       html << html
    #     end
    #     if svg = options[:svg]
    #       html << "<svg xmlns='http://www.w3.org/2000/svg' version='1.1' height='190'>#{svg}</svg>"
    #     end

    Xiki::Browser.html html, :name=>"d3", :via_os=>1
  end

  def self.menu_after output, *args

    # Whenever there's no output, interject

    # If it's quoted, run it in the browser (wrapping in d3 html if no there yet)
    txt = args[-1]

    return "=beg/quoted/" if txt =~ /^\|/   # If |..., ask for quote

    return if txt !~ /\n/   # Return if not multi-line

    self.render txt

    "<* opened in browser!"
  end
end
