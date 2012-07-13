require "mode"

# Makes text in .deck files huge, and makes left and right arrow keys treat
# headings as slides.
class Deck

  def self.menu
    %`
    - .enable arrow keys/
    - docs/
      > Summary
      | Create a file with a ".deck" extension to create a lightweight
      | presentation.  The left and right arrow keys will go back and forth
      | between the "slides".
      |
      | Make .deck files just like you make .notes files, with sections divided
      | by headings ("> foo" lines).  The sections behave like slides.  Only one
      | section is shown at a time.
      |
      > Keys
      | Use these keys to go back and forth between slides:
      | - right+arrow+key: show next slide (hiding everything else)
      | - left+arrow+key: show next slide
      | - custom+reminder: jump to corresponding heading at end
      |
      > Hints
      | To create and show hints that correspond to a section, create a section
      | near the bottom of the file with the same heading but starting with
      | ">>" instead of ">"
      |
      | Then, type open+related+heading to jump back and forth.
      |
      > To enable the deck keys in a .notes file
      - type: do+keys+deck
      - or use) @deck/enable arrow keys/
      |
    `
  end

  @@size = 10

  def self.enable_arrow_keys options={}

    # If in an actual file, just enable for it
    if View.file
      $el.use_local_map $el.elvar.deck_mode_map
      return View.flash "- Enabling arrow keys"
    end

    last = View.files.find{|o| o =~ /\.notes$/}
    basename = File.basename(last)
    View.open last

    View.flash "- Enabling arrow keys!", :times=>4 unless options[:silent]
    $el.use_local_map $el.elvar.deck_mode_map
  end

  def self.keys # mode=:deck_mode_map
    return if ! $el

    $el.elvar.deck_mode_map = $el.make_sparse_keymap unless $el.boundp :deck_mode_map
    $el.set_keymap_parent $el.elvar.deck_mode_map, $el.elvar.notes_mode_map

    Keys.custom_reminder(:deck_mode_map) { Deck.open_related_heading }
    Keys.layout_uncover(:deck_mode_map) {
      View.status nil, :nth=>3
      Hide.reveal
    }

    $el.define_key(:deck_mode_map, $el.kbd("<right>")) { Deck.right_arrow }
    $el.define_key(:deck_mode_map, $el.kbd("<left>")) { Deck.left_arrow }

    # TODO Get this to not add item at top of "Keys > Do" menu bar menu - how?!
    Keys.do_keys_deck { Deck.enable_arrow_keys }
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

  def self.left_arrow

    Notes.narrow_block if ! View.hidden?   # If not hidden, hide first, for simplicity

    column = View.column
    number = View.visible_line_number

    View.visible_line_number = 1

    self.show_all

    Notes.to_block(:up)

    result = self.set_bars

    left, ignore, right = View.block_positions
    lines_in_block = Line.number(right) - Line.number(left)
    line = Line.number

    number = lines_in_block if number > lines_in_block

    View.line = line + number - 1
    View.column = column

    Notes.narrow_block

    Effects.glow :fade_in=>1, :what=>:block if result[0] == 0
  end

  def self.right_arrow

    Notes.narrow_block if ! View.hidden?   # If not hidden, hide first, for simplicity

    column = View.column

    number = View.visible_line_number
    Move.backward if View.bottom == View.cursor   # If at bottom of visible, back up one

    self.show_all

    Notes.to_block

    result = self.set_bars

    Notes.narrow_block

    View.visible_line_number = number

    View.column = column

    Effects.glow :fade_in=>1, :what=>:block if result[2]
  end

  # Sets little bars at bottom of window (mode line)
  def self.set_bars

    first_in_block = false

    my_line_number = View.line

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

    View.status :bars=>[top_bar, bottom_bar]

    [top_bar, bottom_bar, first_in_block]
  end


  def self.init
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
Deck.init   # Define mode
