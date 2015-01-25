$LOAD_PATH << "#{XIKI_DIR}/lib/"

require 'xiki'
Xiki.init

$el.message "" if $el   # Keep it from showing junk at bottom

# $el won't exist the first time through, so exit (we just wanted to load the classes and then fork)...

return if ! $el


module ::Xiki

  KeyShortcuts.keys

  Keys.el4r_init
  KeyShortcuts.right_click   # Use default key shortcuts

  Styles.reload_styles

  Keys.set("M-h") { $el.backward_kill_word 1 }
  if ! $el.display_graphic_p   # for terminal emacs
    # Shouldn't this not have the "<" and ">"?
    Keys.set("<M-RET>") { Launcher.go }
  end

  # Move this into a theme?
  $el.tool_bar_mode(-1) if Environment.gui_emacs

  if Styles.dark_bg?

    Styles.define :ediff_fine_diff_A, :bg => "990000", :fg => "ffffff"
    Styles.define :ediff_current_diff_A, :bg => "440000", :fg => "ff6666"

    Styles.define :ediff_even_diff_A, :bg => "220000", :fg => "dd4444"
    Styles.define :ediff_odd_diff_A, :bg => "221111", :fg => "dd4444"

    Styles.define :ediff_fine_diff_B, :bg => "228800", :fg => "ffffff"
    Styles.define :ediff_current_diff_B, :bg => "2c4400", :fg => "44dd33"

    Styles.define :ediff_even_diff_B, :bg => "0c2200", :fg => "33cc22"
    Styles.define :ediff_odd_diff_B, :bg => "1a2211", :fg => "33cc22"
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

Xiki::Ruby.keys   # Ruby mode shortcuts custom+next and custom+previous

startup_rb = File.expand_path "~/xiki/misc/startup.rb"
load startup_rb if File.exists? startup_rb

