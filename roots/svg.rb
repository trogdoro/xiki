module Xiki
  class Svg
    MENU = %`
      | <!-- Sample svg code to try out -->
      | <polygon points="60,20 300,80 120,240" style="fill:orange;stroke:red;" />
      - .inline/
      - examples/
        - circle/
          | <circle cx="200" cy="50" r="40" style="fill:orange;stroke:red;stroke-width:5;" />
        - rectangle/
          | <rect x="70" y="20" rx="10" ry="10" width="200" height="100" style="fill:rgb(0,0,255);stroke-width:1;stroke:rgb(0,0,0)"/>
        - triangle/
          | <polygon points="60,20 300,80 120,240" style="fill:orange;stroke:red;" />
        - star/
          | <polygon points="100,10 40,180 190,60 10,60 160,180" style="fill:lime;stroke:purple;stroke-width:5;fill-rule:evenodd;" />
        - text/
          | <text x="100" y="50" style="fill:#f00; font-family:arial; font-size:50px;">Hey</text>
          | <text x="100" y="50" style="fill:green; font-family:arial; font-size:50px; text-anchor:end;">Hey</text>
          | <text x="100" y="50" style="fill:orange; font-family:arial; font-size:50px; text-anchor:middle;">Hey</text>
        - image/
          | <image class="shark" xlink:href="http://xiki.loc/shark.svg" width="500" height="500" />
        - gradient/
          | <defs>
          |   <linearGradient id="fade" x1="0%" y1="0%" x2="0%" y2="100%" spreadMethod="pad">
          |     <stop offset="0%" stop-color="#0c0" stop-opacity="1"/>
          |     <stop offset="100%" stop-color="#060" stop-opacity="1"/>
          |   </linearGradient>
          | </defs>
          | <rect x="10" y="10" width="75" height="100" style="fill:url(#fade); stroke: #005000; stroke-width: 4;" />
        - attributes instead of style/
          | <circle cx="100" cy="50" r="40" stroke="black" stroke-width="5" fill="red" />
        - transforms/
          - shrink it down/
            | <g transform="scale(0.3)">
            |   <polygon points="60,20 300,80 120,240" style="fill:orange;stroke:red; stroke-width:10" />
            | </g>
          - inline rotate/
            |   <path transform="rotate(0.548268, 281, 199.5)" id="svg_1" d="m251,155l74,-3l-38,95l-50,-26l14,-66z" stroke-linecap="round" stroke-linejoin="round" stroke-width="12" stroke="#000000" fill="none"/>
        - css/
          | <defs>
          |   <style type="text/css">
          |     #t { fill: #c3c; }
          |   </style>
          | </defs>
          | <text id="t" x="100" y="50" style="font-family:arial; font-size:50px;">Hey</text>
      - api/
        - render in browser/
          @! Svg.render "<polygon points='50,20 190,80' style='stroke:red;' />"
        - save as a .png file/
          @! Svg.to_png "<polygon points='50,20 190,80' style='stroke:red;' />", "/tmp/line.png"
          | Adds the extra code around it and saves to this file:
          @/tmp/line.png
      - see/
        @svg edit/
      `

    def self.menu_after output, *args
      if output
        # Render as they expand
        #         Svg.render output.gsub(/^\|/, '') if output =~ /\A\|/
        return output
      end
      return "=beg/quoted/" if args[-1] =~ /^\|/
      Svg.render args[-1]
    end

    def self.inline
      txt = Tree.siblings.grep(/^\|/).join("\n")
      txt = Tree.unquote txt
      tmp_png = "/tmp/tmp.png"
      self.to_png txt, tmp_png
      Image >> tmp_png
    end

    def self.to_png xml, file

      # Wrap in <svg> tags if doesn't have any
      if xml !~ /<svg /

        xml = %`
          <?xml version="1.0" encoding="UTF-8" standalone="no"?>
          <svg xmlns="http://www.w3.org/2000/svg" version="1.1" height="250" width="350">
          #{xml}
          </svg>
          `.unindent

      end

      tmp_path = "/tmp/tmp.svg"
      File.open(tmp_path, "w") { |f| f << xml }

      `convert #{tmp_path} "#{file}"`

      nil
    end

    def self.render xml, options={}
      Browser.html %`<!DOCTYPE html><html><body>
        <svg xmlns="http://www.w3.org/2000/svg" version="1.1" height="#{options[:height] || "400"}" width="#{options[:width] || "500"}">
        #{xml}
        </svg>
        </body></html>
        `.unindent
    end

    def self.append xml
      Browser.js %`
        $("svg").append('#{xml}');
        $("body").html($("body").html());
        `.unindent
      nil
    end

    # Scale numeric values according to default font size
    def self.scale xml, options={}

      scale = options[:scale] || Styles.get_font_size / 110.0

      white_list = ["height", "width", "x", "y", "rx", "ry", "stroke-width"].inject({}){|o, i| o[i] = true; o}

      # foo="123"...

      xml.gsub!(/([-\w]+)="(\d+)"/) do |match|
        key, val = $1, $2
        val = (val.to_i * scale).round if white_list[key]
        "#{key}=\"#{val}\""
      end

      # 12,34...

      xml.gsub!(/(\d+),(\d+)/) do |match|
        x, y = $1, $2
        "#{(x.to_i * scale).round},#{(y.to_i * scale).round}"
      end

      # font-size:12...

      xml.gsub!(/font-size:(\d+)/) do |match|
        "font-size:#{($1.to_i * scale).round}"
      end

      xml
    end
  end
end
