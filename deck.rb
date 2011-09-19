require "mode"

# Makes text in .deck files huge, and makes left and right arrow keys treat
# headings as slides.
class Deck
  @@size = 10
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

  def self.keys mode=:deck_mode_map
    $el.elvar.deck_mode_map = $el.make_sparse_keymap unless $el.boundp mode

    $el.define_key mode, $el.kbd("<right>") do
      $el.widen; Hide.show
      Notes.to_block
      Notes.expand_block
    end

    $el.define_key mode, $el.kbd("<left>") do
      $el.widen; Hide.show
      Notes.to_block(true)
      Notes.expand_block
    end
  end

  def self.init
    self.define_styles
    self.keys
    # Make deck mode happen for .deck files
    Mode.define(:deck, ".deck") do
      Notes.mode
    end
  end
end
Deck.init   # Define mode
