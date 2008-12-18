class Deck
  @@size = 40
  def self.define_styles
    Styles.define :deck_plain, :size => "+#{@@size}"#, :face => "arial black"
    Styles.define :deck_h1, :fg => 'ffffff', :bg => "333355", :size => "+#{@@size+10}", :face => "arial black"
    Styles.define :deck_h1_label, :fg => '555577', :bg => "333355", :size => "+#{@@size+10}", :face => "arial black"
    Styles.define :deck_bullet, :fg => 'f90', :size => "+#{@@size}", :face => "arial black"
  end

  def self.apply_styles
    # Normal text
    Styles.apply ".+", :deck_plain
    # | Headings
    Styles.apply "^\\(| \\)\\(.*\n\\)", nil, :deck_h1_label, :deck_h1
    # - Bullets
    Styles.apply "^\\( *\\)\\(-\\) \\(.+\\)", nil, :deck_plain, :deck_bullet, :deck_plain
  end

  def self.keys
    $el.elvar.deck_mode_map = $el.make_sparse_keymap unless $el.boundp :deck_mode_map
    Keys.custom_archive(:deck_mode_map) { Deck.archive }

    $el.define_key :deck_mode_map, $el.kbd("<right>") do
      $el.widen; Hide.show
      Notes.to_block
      Notes.expand_block
    end

    $el.define_key :deck_mode_map, $el.kbd("<left>") do
      $el.widen; Hide.show
      Notes.to_block(true)
      Notes.expand_block
    end
  end

  def self.init
    self.keys
    self.define_styles

    # Mode method
    $el.defun(:deck_mode, :interactive => "", :docstring => "Apply deck styles, etc") {
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"
      Deck.apply_styles
      $el.use_local_map $el.elvar.deck_mode_map
    }

    # Associate with .deck file extension
    $el.el4r_lisp_eval %q<
      (add-to-list 'auto-mode-alist '("\\\\.deck\\\\'" . deck-mode))
      >
  end
end
Deck.init   # Define mode
Deck.keys   # Define local keys
