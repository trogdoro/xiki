require 'xiki/core/mode'

module Xiki
  # Makes text in .deck files huge, and makes left and right arrow keys treat
  # headings as slides.
  class Deck

    @@size = 10

    def self.enable_arrow_keys options={}

      self.keys

      $el.use_local_map $el.elvar.deck_mode_map
      "<* Enabling arrow keys"
    end

    def self.keys # mode=:deck_mode_map
      return if ! $el

      $el.elvar.deck_mode_map = $el.make_sparse_keymap unless $el.boundp :deck_mode_map
      $el.set_keymap_parent $el.elvar.deck_mode_map, $el.elvar.notes_mode_map

      $el.define_key(:deck_mode_map, $el.kbd("<S-right>")) { Deck.right_arrow :dont_move=>1 }
      $el.define_key(:deck_mode_map, $el.kbd("<right>")) { Deck.right_arrow }
      $el.define_key(:deck_mode_map, $el.kbd("<left>")) { Deck.left_arrow }

    end

    def self.open_related_heading

      orig = Location.new

      was_hidden = View.hidden?

      Hide.reveal if was_hidden

      left, after_header, right = View.block_positions "^>"
      heading = View.txt left, after_header-1

      delimeter = heading[/^>+/]

      delimeter.size > 1 ?
        heading.sub!(/./, '') :
        heading.sub!(/^/, '>')

      View.to_highest
      found = Search.forward "^#{$el.regexp_quote heading}$"

      if ! found
        orig.go
        return View.flash "- No related heading found"
      end

      Move.to_axis
      View.recenter_top
      Notes.narrow_block(:delimiter=>delimeter == ">" ? ">>" : ">") if was_hidden

    end

    def self.left_arrow options={}

      Notes.narrow_block if ! View.hidden?   # If not hidden, hide first, for simplicity

      column = View.column
      number = View.visible_line_number

      View.visible_line_number = 1

      self.show_all

      Notes.to_block(:up=>1)

      result = self.set_bars

      left, ignore, right = View.block_positions
      lines_in_block = Line.number(right) - Line.number(left)
      line = Line.number

      number = lines_in_block if number > lines_in_block

      View.line = line + number - 1
      View.column = column

      Notes.narrow_block

      Effects.glow(:delay=>0.023, :fade_in=>1, :what=>:block) if ! options[:dont_fade] && result[0] == 0
    end

    def self.right_arrow options={}

      # Make bottom bar blank
      $el.elvar.xiki_bar_hidden = true

      Notes.narrow_block if ! View.hidden?   # If not hidden, hide first, for simplicity

      column = View.column

      number = View.visible_line_number
      Move.backward if View.bottom == View.cursor   # If at bottom of visible, back up one

      self.show_all

      Notes.to_block unless options[:dont_move]

      result = self.set_bars

      Notes.narrow_block

      View.visible_line_number = number

      View.column = column

      Effects.glow :delay=>0.06, :fade_in=>1, :what=>:block if result[2] && ! options[:dont_move] && ! options[:dont_fade]
    end

    # Sets little bars at bottom of window (mode line)
    def self.set_bars

      first_in_block = false

      my_line_number = View.line

      # Not used any more, for now
      top_bar, bottom_bar = 0, 0   # remaining in group, remaining total

      header, header_match_count, my_header = nil, 0, nil
      View.txt.split("\n").each_with_index do |line, i|
        i = i + 1
        is_header = line =~ /^> /
        if is_header

          if header == line   # If same header as last
            header_match_count += 1
          else
            header_match_count = 1
            header = line
          end

          if my_header   # If we found it, start accumulating
            top_bar += 1 if header == my_header
            bottom_bar += 1
          end
        end

        # If line matched, remember it's this header
        if i == my_line_number # && ! my_line_number
          my_header = header
          first_in_block = true if header_match_count == 1
          top_bar, bottom_bar = 0, 0   # remaining in group, remaining total
        end

      end

      [top_bar, bottom_bar, first_in_block]
    end

    def self.init_in_client
      self.keys
      # Make deck mode happen for .deck files
      Mode.define(:deck, ".deck") do
        Notes.mode
        $el.use_local_map $el.elvar.deck_mode_map   # Adds arrow keys onto notes map
      end
    end

    def self.show_all
      $el.widen
      Hide.show
    end

  end
end
