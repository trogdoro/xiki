class Xiki::Animate
  MENU = "
    - sequences/
      - bouncing ball/
        |~ a
        |~ a
        |~ a
        |~ b
        |~ b
        |~ c
        |~ d
        |~ e
        |~ d
        |~ c
        |~ b
        |~ b
        |~ a
        |~ a
        |~ a
        |~ b
        |~ b
        |~ c
        |~ d
        |~ e
        |~ d
        |~ c
        |~ c
        |~ b
        |~ b
        |~ b
        |~ c
        |~ c
        |~ d
        |~ e
        |~ d
        |~ d
        |~ e
        |~ d
        |~ e
      - explosion/
        |~ a
        |~ a
        |~ a
        |~ b
        |~ b
        |~ b
        |~ c
        |~ c
        |~ d
        |~ e
        |~ ii
        |~ vvv
        |~ |||v
        |~ exnxe
        |~ y ad c
        |~ e c
        |~ e
      - gun/
        |~ eiyrsssbbbbfb
        |~ eiyrsssbbbbfbb
        |~ eiyrsssbbbbfb     b
        |~ eiyrsssbbbbfb          b
        |~ eiyrsssbbbbfb               b
        |~ eiyrsssbbbbfb                    b
        |~ eiyrsssbbbbfb                         b
        |~ eiyrsssbbbbfb                              b
        |~ eiyrsssbbbbfb                                   b
        |~ eiyrsssbbbbfb
      - ship/
        |~ dd
        |~ iidd
        |~ iiiidd
        |~ iiiiiidd
        |~ |iiiiiiidd
        |~ ii|iiiiiiidd
        |~ viii|iiiiiiidd
        |~ |vviii|iiiiiiidd
        |~ i||vviii|iiiiiiidd
        |~ iii||vviii|iiiiiiidd
        |~ viiii||vviii|iiiiiiidd
        |~ vvviiii||vviii|iiiiiiidd
        |~ vvvvviiii||vviii|iiiiiiidd
        |~ iivvvvviiii||vviii|iiiiiiidd
        |~ iiiivvvvviiii||vviii|iiiiiiidd
        |~ ddiiiivvvvviiii||vviii|iiiiiiidd
        |~  ddiiiivvvvviiii||vviii|iiiiiiidd
        |~   ddiiiivvvvviiii||vviii|iiiiiiidd
        |~    ddiiiivvvvviiii||vviii|iiiiiiidd
        |~     ddiiiivvvvviiii||vviii|iiiiiiidd
        |~      ddiiiivvvvviiii||vviii|iiiiiiidd
        |~       ddiiiivvvvviiii||vviii|iiiiiiidd
        |~        ddiiiivvvvviiii||vviii|iiiiiiidd
      - space invaders/
        |~ dtprppprptd  ehcsrsssrsche
        |~ edgprppprpgde  hlsrsssrslh
        |~  dtprppprptd  ehcsrsssrsche
        |~  edgprppprpgde  hlsrsssrslh
        |~   dtprppprptd  ehcsrsssrsche
        |~   edgprppprpgde  hlsrsssrslh
        |~    dtprppprptd  ehcsrsssrsche
        |~    edgprppprpgde  hlsrsssrslh
        |~     dtprppprptd  ehcsrsssrsche
        |~     edgprppprpgde  hlsrsssrslh
        |~      dtprppprptd  ehcsrsssrsche
        |~      edgprppprpgde  hlsrsssrslh
        |~       dtprppprptd  ehcsrsssrsche
        |~       edgprppprpgde  hlsrsssrslh
        |~        dtprppprptd  ehcsrsssrsche
        |~        edgprppprpgde  hlsrsssrslh
        |~         dtprppprptd  ehcsrsssrsche
        |~         edgprppprpgde  hlsrsssrslh
        |~          dtprppprptd  ehcsrsssrsche
        |~          edgprppprpgde  hlsrsssrslh
        |~           dtprppprptd  ehcsrsssrsche
        |~           edgprppprpgde  hlsrsssrslh
        |~            dtprppprptd  ehcsrsssrsche
        |~            edgprppprpgde  hlsrsssrslh
        |~             dtprppprptd  ehcsrsssrsche
        |~             edgprppprpgde  hlsrsssrslh
        |~              dtprppprptd  ehcsrsssrsche
        |~              edgprppprpgde  hlsrsssrslh
        |~               dtprppprptd  ehcsrsssrsche
        |~               edgprppprpgde  hlsrsssrslh
        |~                dtprppprptd  ehcsrsssrsche
        |~                edgprppprpgde  hlsrsssrslh
        |~                 dtprppprptd  ehcsrsssrsche
        |~                 edgprppprpgde  hlsrsssrslh
        |~                edgprppprpgde  hlsrsssrslh
        |~                dtprppprptd  ehcsrsssrsche
        |~               edgprppprpgde  hlsrsssrslh
        |~               dtprppprptd  ehcsrsssrsche
        |~              edgprppprpgde  hlsrsssrslh
        |~              dtprppprptd  ehcsrsssrsche
        |~             edgprppprpgde  hlsrsssrslh
        |~             dtprppprptd  ehcsrsssrsche
        |~            edgprppprpgde  hlsrsssrslh
        |~            dtprppprptd  ehcsrsssrsche
        |~           edgprppprpgde  hlsrsssrslh
        |~           dtprppprptd  ehcsrsssrsche
        |~          edgprppprpgde  hlsrsssrslh
        |~          dtprppprptd  ehcsrsssrsche
        |~         edgprppprpgde  hlsrsssrslh
        |~         dtprppprptd  ehcsrsssrsche
        |~        edgprppprpgde  hlsrsssrslh
        |~        dtprppprptd  ehcsrsssrsche
        |~       edgprppprpgde  hlsrsssrslh
        |~       dtprppprptd  ehcsrsssrsche
        |~      edgprppprpgde  hlsrsssrslh
        |~      dtprppprptd  ehcsrsssrsche
        |~     edgprppprpgde  hlsrsssrslh
        |~     dtprppprptd  ehcsrsssrsche
        |~    edgprppprpgde  hlsrsssrslh
        |~    dtprppprptd  ehcsrsssrsche
        |~   edgprppprpgde  hlsrsssrslh
        |~   dtprppprptd  ehcsrsssrsche
        |~  edgprppprpgde  hlsrsssrslh
        |~  dtprppprptd  ehcsrsssrsche
        |~ edgprppprpgde  hlsrsssrslh
        |~ dtprppprptd  ehcsrsssrsche
    "

  def self.menu_after output, *args

    # If root, add sequences
    if args == []
      #       sequences = Tree.children self.menu.unindent, "sequences"
      sequences = Tree.children MENU.unindent, "sequences"
      return "#{sequences}#{output}"
    end

    return if output

    sequence = args[0]

    sequence = Tree.children MENU.unindent, "sequences/#{sequence}"

    sequence = sequence.split "\n"

    cursor = View.cursor
    indent = Line.indent
    Move.to_end
    View.<< "\n", :dont_move=>1

    sequence.each do |line|
      Line.next
      Line.sub! /.*/, "  #{indent}#{line}"
      View.cursor = cursor;
      View.pause 0.05;
    end

    nil
  end

end
