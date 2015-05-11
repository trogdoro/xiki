$LOAD_PATH << "#{XIKI_DIR}/lib/"

require 'xiki'
Xiki.init

$el.message "" if $el   # Keep it from showing junk at bottom

module ::Xiki

  # We're in the initial run, so pre-load and cache some stuff and exit...

  # Ruby classes are loading without an editor. # $el won't exist the first
  # time through, so exit (we just wanted to load the classes and then fork)

  if ! $el

    # Make caching version of $el, and generate some lisp for improved subsequent startup time
    $el = El4r::ELInstance.new nil, nil, StringIO.new
    KeyShortcuts.search_keys
    Color.define_styles
    Effects.define_styles
    FileTree.define_styles
    Notes.define_styles
    Notes.apply_styles

    return
  end

  KeyShortcuts.keys
  Keys.el4r_init
  KeyShortcuts.right_click   # Use default key shortcuts

  Keys.set("M-h") { $el.backward_kill_word 1 }
  if ! $el.display_graphic_p   # for terminal emacs
    # Shouldn't this not have the "<" and ">"?
    Keys.set("<M-RET>") { Launcher.go }
  end

  # Move this into a theme?
  $el.tool_bar_mode(-1) if Environment.gui_emacs

  if Styles.dark_bg?

    $el.el4r_lisp_eval %<(progn
      (set-face-attribute (make-face 'ediff-fine-diff-A) nil :background "#990000" :foreground "#ffffff")
      (set-face-attribute (make-face 'ediff-current-diff-A) nil :background "#440000" :foreground "#ff6666")

      (set-face-attribute (make-face 'ediff-even-diff-A) nil :background "#220000" :foreground "#dd4444")
      (set-face-attribute (make-face 'ediff-odd-diff-A) nil :background "#221111" :foreground "#dd4444")

      (set-face-attribute (make-face 'ediff-fine-diff-B) nil :background "#228800" :foreground "#ffffff")
      (set-face-attribute (make-face 'ediff-current-diff-B) nil :background "#2c4400" :foreground "#44dd33")

      (set-face-attribute (make-face 'ediff-even-diff-B) nil :background "#0c2200" :foreground "#33cc22")
      (set-face-attribute (make-face 'ediff-odd-diff-B) nil :background "#1a2211" :foreground "#33cc22")
    )>
  end

end

elvar.javascript_mode_map = make_sparse_keymap unless $el.boundp :javascript_mode_map

if $el.boundp(:ruby_mode_map)
  define_key :ruby_mode_map, kbd("C-c C-e") do
    Xiki::View.insert "end"
    $el.ruby_indent_line
  end
end

module ::Xiki

  $el.el4r_lisp_eval "(set-frame-parameter nil 'alpha '(100 100))"

  if true
    Styles.define :isearch, :bg=>"0c0", :fg=>"bfb", :bold=>1   # Light Green

    Styles.define :lazy_highlight, :bg=>"006", :fg=>"33f", :bold=>1   # Blue
  else
    Styles.define :isearch, :bg=>"3b0", :fg=>"fff", :bold=>1
    Styles.define :lazy_highlight, :bg=>"8d7", :fg=>"fff", :bold=>1
  end

end

elvar.sort_fold_case = true   # To avoid stupid bug

module ::Xiki
  Ruby.keys   # Ruby mode shortcuts custom+next and custom+previous

  # Try loading user's startup.rb if it exists
  startup_rb = File.expand_path "~/xiki/misc/startup.rb"

  Code.load startup_rb if File.exists? startup_rb
end

