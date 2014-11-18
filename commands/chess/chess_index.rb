class Xiki::Chess

  NEW_BOARD = "
        |:tMvWlVmT
        |:OoOoOoOo
        |: + + + +
        |:+ + + + 
        |: + + + +
        |:+ + + + 
        |:pPpPpPpP
        |:RnBqKbNr"

  MENU = %`
    - games/
    - puzzles/
      - checkmate in 2/
        |: + + B +
        |:+ + + R 
        |: T + + L
        |:+t+o+r+ 
        |:o+ + +pO
        |:P P + M 
        |: P + +p+
        |:+ + + K 
    - boards/
      - new/#{NEW_BOARD}
      - other idea for notation/
        | UZVQKVZU
        | OOOOOOOO
        |  + + + +
        | + + + + 
        |  + + + +
        | + + + + 
        | oooooooo
        | uzvqkvzu
      - all pieces/
        |:tmvwl
        |:o
        |:p
        |:rnbqk
    - web fonts/
      - Leipzig/
        http://fontsforweb.com/public/fonts/658/LEIPFONT.ttf
      - Condal/
        http://fontsforweb.com/public/fonts/759/CONDFONT.ttf
      - Marroquin/
        http://fontsforweb.com/public/fonts/701/MARRFONT.ttf
      - Cases/
        http://fontsforweb.com/public/fonts/602/CASEFONT.ttf
      - Lucena/
        http://fontsforweb.com/public/fonts/715/LUCEFONT.ttf
    - .play/
    `


  def self.mode
    $el.elvar.chess_mode_map = $el.make_sparse_keymap unless $el.boundp :chess_mode_map
    $el.set_keymap_parent $el.elvar.chess_mode_map, $el.elvar.notes_mode_map
    $el.define_key(:chess_mode_map, $el.kbd("<mouse-1>"), :xiki_chess_mouse_click)

    $el.use_local_map $el.elvar.chess_mode_map
    Notes.keys
  end

  def self.styles
    Xiki["themes/list/White BG/"]

    Styles.define :chess, :size=>'+45', :face=>"Chess Cases"
    Styles.define :chess_active, :size=>'+45', :face=>"Chess Cases", :fg=>"#3f3"
    Styles.define :chess_small, :size=>'-7', :face=>"Chess Cases", :fg=>"#ddd"
    Styles.apply("^\\( *\\)\\(|\\(:\\)\\)\\(.+\n\\)", nil, :chess_small, :chess_small, :chess, :chess)
    Styles.define :trailing_whitespace, :bg=>'fff'
    Cursor.underscore
  end

  def self.define_chess_mouse_click

    $el.defun(:xiki_chess_mouse_click, :interactive => "e") do |e|

      $el.mouse_set_point(e)

      next if Line !~ /^ *\|:/

      # Remember if old location was lit up
      prev_lit = Overlay.at($el.elvar.xiki_chess_last_point).any?
      # ::Mark.clear
      Overlay.delete_all

      # If current same as previous, do nothing (we unlit it)
      if $el.elvar.xiki_chess_last_point == View.cursor
        $el.elvar.xiki_chess_last_point = 0
        return
      end

      # If one was prev_lit, do the move...

      if prev_lit
        current = View.cursor

        prev = $el.elvar.xiki_chess_last_point
        View.cursor = prev
        o = View.delete(prev, prev+1)
        View.<< o =~ /[A-Z]/ ? "+" : " "

        View.cursor = current
        captured = View.delete(View.cursor, View.cursor+1)
        o = captured == "+" || captured =~ /[A-Z]/ ? o.upcase : o.downcase
        View.<< o, :dont_move=>1

        $el.elvar.xiki_chess_last_point = 0

        next
      end

      # If just a blank square, do nothing
      if [" ", "+"].member?(View.txt(View.cursor, View.cursor+1))
        $el.elvar.xiki_chess_last_point = 0
        next
      end


      # None was lit, so just select...
      over = $el.make_overlay(View.cursor, View.cursor+1)
      $el.overlay_put over, :face, :chess_active

      $el.elvar.xiki_chess_last_point = View.cursor

    end
  end

  def self.play

    self.mode
    self.styles

    $el.elvar.xiki_chess_last_point = 0;

    Xiki::Chess::NEW_BOARD.gsub(/^ */, "")
  end


  def self.render txt, options={}

    txt ||= NEW_BOARD.strip.gsub(/^ *\|:/, "")

    txt = txt.gsub(/^\|:/, "")
    txt.gsub!(/^: /, "")

    # If 15 wide, space every other piece
    txt.gsub!(/^.{15}$/){|o| "#{o}".gsub(/(.) /, "\\1")}

    txt.gsub! /$/, "<br>"
    txt.gsub! " ", "&nbsp;"

    #     font_url = options[:font_url] || "http://fontsforweb.com/public/fonts/602/CASEFONT.ttf"
    font_url = options[:font_url] || "http://fontsforweb.com/public/fonts/658/LEIPFONT.ttf"
    name = "chess#{rand 1000}"
    Browser.html %`
      <style type="text/css">
          @font-face{
            font-family: "#{name}";
            src: url('#{font_url}') format("truetype");
          }
          .#{name} { font-family: "#{name}"; }
          body { font-size: 45px }
      </style>
      <div class=#{name}>
        #{txt}
      </div>
      `
    nil
  end

  def self.menu_after output, category=nil, name=nil, input=nil

    # /web fonts/foo, so render it...

    if category == "web fonts" && name   # Do nothing if not /web fonts/foo
      self.render nil, :font_url=>output.strip
      return "<! rendered in browser!"
    end
  end

  # Convert from "FEN" format that chess gem uses
  def self.clean_specials
    "hi"
  end

  # ChessIndex.chess_font_installed
  def self.chess_font_installed
    $el.assoc("chess cases", $el.x_font_family_list)
  end

end
