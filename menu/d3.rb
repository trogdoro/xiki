class D3

  # v3 makes this not show the movement
  # @d3/examples/movement/position/
  # We're at .v2 now - go back to .v3 when this is fixed?
  # <script type="text/javascript" src="http://d3js.org/d3.v2.min.js"></script>
  # <script type="text/javascript" src="http://d3js.org/d3.v3.min.js"></script>

  MENU = %`
    | // Sample d3 code to modify and run
    | svg.append("rect").attr("x", 40).attr("y", 40).attr("width", 60).attr("height", 60);
    - examples/
      - hello world/
        | svg.append("rect")
        |   .attr("x", 60)
        |   .attr("y", 60)
        |   .attr("width", 60)
        |   .attr("height", 60);
      - standalone/
        | Includes include and required html tags.
        @html/
          | <script type="text/javascript" src="http://d3js.org/d3.v2.min.js"></script>
          |
          | <div id="svg"></div>
          | <script>
          |   var svg = d3.select("#svg").append("svg").attr("width", 800).attr("height", 800);
          |   svg.append("rect")
          |     .attr("x", 60)
          |     .attr("y", 60)
          |     .attr("width", 60)
          |     .attr("height", 60);
          | </script>
      - shapes/
        - square/
          | svg.append("rect")
          |   .attr("x", 60)
          |   .attr("y", 60)
          |   .attr("width", 60)
          |   .attr("height", 60);
        - circle/
          | svg.append("circle")
          |   .attr("cx", 100).attr("cy", 100)
          |   .attr("r", 40)
        - text/
          | svg.append("svg:text")
          |   .text("hello")
          |   .attr("x", 100).attr("y", 100)
          - centered/
            | svg.append("circle")
            |   .attr("cx", 100).attr("cy", 100)
            |   .attr("fill", "#eee")
            |   .attr("r", 50)
            | svg.append("svg:text")
            |   .text("word")
            |   .attr("x", 100).attr("y", 100)
            |   .attr("text-anchor", "middle")
      - text/
        | svg.append("svg:text")
        |   .text("Whatevs").attr("class", "lightweight")
        |   .attr("x", 330).attr("y", 200)
        |   .attr("style", "fill:#fb7; font-size:100px; font-family:arial; font-weight:bold; text-anchor:middle;")
      - scale/
        | svg.append("rect").attr("x", 40).attr("y", 40).attr("width", 60).attr("height", 60)
        | d3.selectAll("rect").transition().attr("transform", "scale(2)").duration(800)
        - in place/
          | svg.append("svg:text")
          |   .text("Whatevs").attr("x", 330).attr("y", 200).attr("style", "fill:#fb7; font-size:100px; font-family:arial; font-weight:bold; text-anchor:middle;")
          |   .transition()
          |     .attr("transform", "scale(1.5) translate(-100, -50)").duration(1800)
        - and move/
          | svg.append("svg:text")
          |   .text("Whatevs").attr("x", 600).attr("y", 200).attr("style", "fill:#fb7; font-size:100px; font-family:arial; font-weight:bold; text-anchor:middle;")
          |   .transition()
          |     .attr("transform", "scale(1.5) translate(-300, -50)").duration(1800)
      - movement/
        - position/
          | var o = svg.append("rect").attr("width", 60).attr("height", 60);
          | o.transition().attr("x", 220);
        - width/
          | var o = svg.append("rect").attr("width", 60).attr("height", 60);
          | o.transition().attr("width", 220);
        - color/
          | var o = svg.append("rect").attr("width", 60).attr("height", 60);
          | o.transition().style("fill", "orange");
          - opacity/
            | var o = svg.append("rect").attr("width", 60).attr("height", 60);
            | o.transition().style("opacity", 0.2);
        - duration/
          | var o = svg.append("rect").attr("width", 60).attr("height", 60);
          | // Take 2 seconds
          | o.transition().attr("x", 220).duration(2000);
          - delay/
            | var o = svg.append("rect").attr("width", 60).attr("height", 60);
            | // Wait 1 second before doing it
            | o.transition().attr("x", 220).delay(1000);
        - curve/
          | svg.append("rect").attr("width", 60).attr("height", 60)
          |   .transition().attr("x", 220).duration(1400)
          |   .transition().attr("y", 220).duration(1000)
          - using a delay/
            | svg.append("rect").attr("width", 60).attr("height", 60)
            |   .transition().attr("x", 220).duration(1400)
            |   .transition().attr("y", 220).duration(1400).delay(300)
        - easing/
          | svg.append("rect").attr("width", 60).attr("height", 60)
          |   // elastic
          |   .transition().attr("x", 220).duration(3000).ease("elastic")
          - linear/
            | svg.append("rect").attr("width", 60).attr("height", 60)
            |   .transition().attr("x", 220).duration(1700).ease("linear")
        - chaining/
          | svg.append("rect").attr("width", 60).attr("height", 60)
          |   .transition()
          |   .attr("x",320)
          |   .each("end", function(){ d3.select(this).
          |     transition().attr("y",180);
          |    });
          - 3 moves/
            | svg.append("rect").attr("width", 60).attr("height", 60)
            |   .transition()
            |   .attr("x", 320)
            |   .each("end", function(){ d3.select(this).
            |     transition().attr("y", 180)
            |       .each("end", function(){ d3.select(this).
            |         transition().attr("x", 0)
            |       })
            |   })
      - effects/
        - with hover/
          | <script type="text/javascript" src="http://d3js.org/d3.v2.min.js"></script>
          |
          | <div id="svg"></div>
          | <script>
          |   var svg = d3.select("#svg").append("svg").attr("width", 100).attr("height", 100);
          |
          |   svg.append("circle")
          |     .style("stroke", "gray").style("fill", "white")
          |     .attr("r", 40).attr("cx", 50).attr("cy", 50)
          |     .on("mouseover", function(){d3.select(this).style("fill", "aliceblue");})
          |     .on("mouseout", function(){d3.select(this).style("fill", "white");});
          | </script>
        - with animation/
          | svg.append("circle")
          |   .style("stroke", "gray").style("fill", "white")
          |   .attr("r", 40).attr("cx", 50).attr("cy", 50)
          |   .transition().delay(100).duration(1000)
          |   .attr("r", 10).attr("cx", 30).style("fill", "black");
      - basics/
        - selecting/
          | // Create an element
          | svg.append("rect").attr("width", 40).attr("height", 40)
          | r = svg.selectAll("rect");   // Get a reference to it
          | r.transition().attr("y", 111)
        - set attribute with a function/
          | c = svg.append("circle").attr("cx", 100).attr("cy", 100).attr("r", 40)
          | c.attr("r", function() {
          |   return Math.random() * 100;
          | });
      - data/
        - bind numbers/
          | svg.append("circle").attr("cx", 50).attr("cy", 100).attr("r", 10)
          | svg.append("circle").attr("cx", 150).attr("cy", 100).attr("r", 10)
          | circle = svg.selectAll("circle");   // Get a reference to it
          |
          | // Bind a number with each circle, respectively
          | circle.data([120, 80]);
          |
          | // Set an attribute based on the number
          | circle.attr("cy", function(d) { return d; });
        - by index/
          | svg.append("circle").attr("cx", 50).attr("cy", 100).attr("r", 10)
          | svg.append("circle").attr("cx", 100).attr("cy", 100).attr("r", 10)
          | svg.append("circle").attr("cx", 150).attr("cy", 100).attr("r", 10)
          | circle = svg.selectAll("circle");   // Get a reference to it
          |
          | // Set attribute based on the number
          | circle.attr("cy", function(d, i) { return 100 + i*10; });
        - creating from data/
          | // Binding data to shapes before they exist then calling the
          | // .enter() method, is called using the "enter selection" in d3
          | // terms.
          | svg.selectAll("circle")
          |   .data([10, 20, 30, 40, 20, 70, 30, 12, 50])
          |   .enter()
          |   .append("circle")
          |   .attr("cx", function(d, i) { return (i+1)*30; })
          |   .attr("cy", function(d) { return 100-d; })
          |   .attr("r", 4);
        - bind hashes/
          | // In this case the data are just coordinates to plot
          | svg.selectAll("circle")
          |   .data([{x:100, y:20}, {x:120, y:50}, {x:140, y:30}])
          |   .enter()
          |   .append("circle")
          |   .attr("cx", function(o) { return o.x; })
          |   .attr("cy", function(o) { return o.y; })
          |   .attr("r", 4);
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
        <script type="text/javascript" src="http://d3js.org/d3.v2.min.js"></script>
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

    Xiki::Browser.html html, :name=>"d3"
  end

  def self.menu_after output, *args

    # Whenever there's no output, interject

    # If it's quoted, run it in the browser (wrapping in d3 html if no there yet)
    txt = args[-1]

    return "@beg/quoted/" if txt =~ /^\|/   # If |..., ask for quote

    return if txt !~ /\n/   # Return if not multi-line

    self.render txt

    "@flash/- opened in browser!"
  end
end
