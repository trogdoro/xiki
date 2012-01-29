require "mode"

# Makes text in .deck files huge, and makes left and right arrow keys treat
# headings as slides.
class Deck

  def self.menu
    %`
    - .enable arrow keys/
    - docs/
      > Summary
      | Use a .notes file like a lightweight presentation. Present it via emacs.
      | The sections behave like slides. Use the arrow keys navigate between them.
      | Only one section is shown at a time.
      |
      > Enabling the arrow keys
      | Create a .notes file to use like a presentation, then:
      | - 1) Type as+quick+presentation to give it the "p" quick bookmark
      | - 2) Type open+quick+presentation to jump to it in presentation mode
      |
      | - or use) @deck/enable arrow keys/
      |
      > Keys
      | Use these keys to go back and forth between slides:
      | - left arrow key
      | - right arrow key
      |
      > Hints
      | To get hints to pop up temporarily when moving between slides, create a file
      | named like this, and create blocks with corresponding headings:
      | - my_presentation.hints.notes
      |
      > Using with .deck files
      | Deprecated. Use with .notes files per above.
    `
  end

  @@size = 10

  def self.enable_arrow_keys
    last = View.files.find{|o| o =~ /\.notes$/}
    basename = File.basename(last)

    View.flash "- Enabling for '#{basename}'..."

    View.open last
    $el.use_local_map $el.elvar.deck_mode_map
  end

  def self.use_keys
    $el.use_local_map $el.elvar.deck_mode_map
  end

  def self.define_styles
    Styles.define :deck_plain, :size => "+#{@@size}"#, :face => "arial black"
    Styles.define :deck_h1, :fg => 'ffffff', :bg => "333355", :size => "+#{@@size+10}", :face => "arial black"
    Styles.define :deck_h1_label, :fg => '555577', :bg => "333355", :size => "+#{@@size+10}", :face => "arial black"
    Styles.define :deck_bullet, :fg => 'f90', :size => "+#{@@size}", :face => "arial black"
  end

  def self.apply_styles
    # Normal text
    Styles.apply ".+", :deck_plain
    # | Headings and - Bullets
    Styles.apply "^\\(| \\)\\(.*\n\\)", nil, :deck_h1_label, :deck_h1
    Styles.apply "^\\( *\\)\\(-\\) \\(.+\\)", nil, :deck_plain, :deck_bullet, :deck_plain
  end

  def self.keys # mode=:deck_mode_map
    $el.elvar.deck_mode_map = $el.make_sparse_keymap unless $el.boundp :deck_mode_map
    $el.set_keymap_parent $el.elvar.deck_mode_map, $el.elvar.notes_mode_map

    Keys.open_related_heading { Deck.open_related_heading }

    $el.define_key(:deck_mode_map, $el.kbd("<right>")) { Deck.right_arrow }

    $el.define_key(:deck_mode_map, $el.kbd("<left>")) { Deck.left_arrow }
  end

  def self.init
    self.define_styles
    self.keys
    # Make deck mode happen for .deck files
    Mode.define(:deck, ".deck") do
      Notes.mode
      $el.use_local_map $el.elvar.deck_mode_map   # Adds arrow keys onto notes map
    end
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

end
Deck.init   # Define mode
