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
      | Either use @deck/enable arrow keys/ or do this:
      |
      | 1) Create a .notes file to use like a presentation
      | 2) Type as+quick+presentation to give it the "p" quick bookmark
      | 3) Type open+quick+presentation to jump to it in presentation mode
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

    $el.define_key :deck_mode_map, $el.kbd("<right>") do
      Deck.right_arrow
    end

    $el.define_key :deck_mode_map, $el.kbd("<left>") do
      Deck.left_arrow
    end
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

  def self.left_arrow
    $el.widen
    Hide.show

    Notes.to_block(true)
    Notes.narrow_block

    self.show_hint
  end

  def self.right_arrow
    $el.widen
    Hide.show
    Notes.to_block
    Notes.narrow_block

    self.show_hint
  end

  def self.show_hint
    # Get heading
    left, after_header, right = View.block_positions "^>"
    heading = View.txt left+2, after_header-1

    # Get heading from .hints.notes file
    file = View.file.sub /.notes$/, ".hints\\0"
    hint = Notes.read_block file, "> #{heading}" rescue nil

    hint.strip! if hint.present?

    return if hint.blank?

    View.cursor = right-1
    View << "#{hint}\n"
    View.cursor = left

    $el.sit_for 1.5

    View.cursor = right-1
    Line.delete
    View.cursor = left
  end

end
Deck.init   # Define mode
