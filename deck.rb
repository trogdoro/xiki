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
      | - open+related+heading: jump to corresponding hint
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
    $el.elvar.deck_mode_map = $el.make_sparse_keymap unless $el.boundp :deck_mode_map
    $el.set_keymap_parent $el.elvar.deck_mode_map, $el.elvar.notes_mode_map

    # TODO Get this to not add key at beginning - how?!
    Keys.open_related_heading { Deck.open_related_heading }

    $el.define_key(:deck_mode_map, $el.kbd("<right>")) { Deck.right_arrow }
    $el.define_key(:deck_mode_map, $el.kbd("<left>")) { Deck.left_arrow }

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

    $el.widen
    Hide.show

    Notes.to_block(:up)
    left, ignore, right = View.block_positions
    lines_in_block = Line.number(right) - Line.number(left)
    line = Line.number

    number = lines_in_block if number > lines_in_block

    View.line = line + number - 1
    View.column = column

    Notes.narrow_block
  end

  def self.right_arrow

    Notes.narrow_block if ! View.hidden?   # If not hidden, hide first, for simplicity

    column = View.column

    number = View.visible_line_number
    Move.backward if View.bottom == View.cursor   # If at bottom of visible, back up one

    $el.widen
    Hide.show
    Notes.to_block

    Notes.narrow_block

    View.visible_line_number = number

    View.column = column

  end

  def self.init
    self.keys
    # Make deck mode happen for .deck files
    Mode.define(:deck, ".deck") do
      Notes.mode
      $el.use_local_map $el.elvar.deck_mode_map   # Adds arrow keys onto notes map
    end
  end

end
Deck.init   # Define mode
