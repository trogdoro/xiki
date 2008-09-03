require 'strscan'
require 'hide'
require 'keys'

# Currently not used
class Xiki
  extend ElMixin
  @@default_server = "localhost:11111"
#  @@default_server = "xiki.org"
#  @@default_server = "http://#{@@default_server_root}"
  @@xiki_dir = File.expand_path("~/.xiki")
  @@original_dir = "#{@@xiki_dir}/orig"
  @@hook_enabled = true
  @@history = []

  # Run when any file is open.  We do something if it ends in .xiki.
  def self.run_upon_file_open

    # If not a .xiki or .xik file, exit
    return unless buffer_file_name and parse_page_name_from_file

    if in_orig_dir
      insert "Error\n- You shouldn't open pages from this dir" if buffer_string.size == 0
      return
    end

    self.add_to_history_maybe

    toggle_read_only 0

    erase_buffer
    txt = get_page_from_server parse_page_name_from_file
    insert txt
    goto_line 3
## TODO can I just use the read-only state to figure out what to do?
##  - buffer-read-only
#    elvar.xiki_state = :read_only

    toggle_read_only 1

    save_original_version

    save_buffer


#    message buffer_file_name

  #  insert xiki
  #  return unless()
  #insert(buffer_file_name =~ /(.+)\.xiki$/)
  #  message "-> it's xiki"
  end

  # Run when emacs is loaded
  def self.startup

    define_styles

    # Run this upon empty files
#    defun(:xiki_run_upon_file_open, :interactive => "", :docstring => "Runs upon open") do
#      Xiki.run_upon_file_open
    #end
    el4r_lisp_eval "
      (defun xiki-run-upon-file-open ()
        (if (member \"*el4r:process*\" (mapcar 'buffer-name (buffer-list)))
          (el4r-ruby-eval \"Xiki.run_upon_file_open\")
          (message \"Xiki couldn't check this file because el4r is dead :(\")
        )
      )
    "
    add_hook :find_file_hook, :xiki_run_upon_file_open

    defun(:xiki_mode, :interactive => "", :docstring => "Mode for .xiki extention") do
      Xiki.define_mode
    end
    el4r_lisp_eval %q/
      (progn
        (add-to-list 'auto-mode-alist '("\\\\.xiki\\\\'" . xiki-mode))
        (add-to-list 'auto-mode-alist '("\\\\.xik\\\\'" . xiki-mode)))
      /

    define_global_shortcuts

  end

  def self.define_mode

    elvar.mode_name = "Xiki"
    elvar.major_mode = :xiki_mode

    fontify

    define_keys
    use_local_map elvar.xiki_mode_keymap_read_only



#    use_local_map elvar.xiki_mode_keymap

    # TODO put faces here


#    # Template > Defining a key
#    Keys.XS(:xiki_mode_keymap) do
#      Xiki.save
#    end

  end

  def self.define_keys
    elvar.xiki_mode_keymap = make_sparse_keymap
    elvar.xiki_mode_keymap_read_only = make_sparse_keymap

    set_keymap_parent elvar.xiki_mode_keymap_read_only, elvar.xiki_mode_keymap

    # Template > Defining a key
    Keys.XS(:xiki_mode_keymap) do
      Xiki.save
    end

    # Set a-z to open link
    ("a".."z").each do |l|
      define_key :xiki_mode_keymap_read_only, l do
        lowercase_key_typed l
      end
    end

    # Set a-z to open link
    ("A".."Z").each do |l|
      define_key :xiki_mode_keymap_read_only, l do
        Xiki.uppercase_key_typed l
      end
    end

    # Set a-z to open link
    ("1".."9").each do |l|
      define_key :xiki_mode_keymap_read_only, l do
        Xiki.number_typed l
      end
    end

    define_key :xiki_mode_keymap_read_only, "\e" do
      Hide.show
      beginning_of_buffer
      next_line 2
    end

    define_key :xiki_mode_keymap_read_only, "0" do
      Hide.show
      beginning_of_buffer
      next_line 2
    end

    define_key :xiki_mode_keymap_read_only, " " do
      Xiki.search_in_headers
    end

    define_key :xiki_mode_keymap_read_only, "," do
      Xiki.allow_editing
    end

    # Make . goto home page
    define_key :xiki_mode_keymap_read_only, "/" do
      Xiki.open_page "home"
    end

    # Back
    define_key :xiki_mode_keymap_read_only, "[" do
      self.back
    end
    # Forward
    define_key :xiki_mode_keymap_read_only, "." do
      insert "-> implement: forward"
    end


    # TODO Set A-Z to search in headings

    Keys.M :xiki_mode_keymap do
      if elvar.buffer_read_only
        Xiki.wiki_click true
      else
        insert "\n"
      end
    end

    define_key :xiki_mode_keymap, kbd("<mouse-1>") do
      Xiki.wiki_click
#      insert "--> hey"
    end

    Keys.CE(:xiki_mode_keymap) { Xiki.allow_editing }
    
    Keys.CR(:xiki_mode_keymap) do
      # If modified, prompt for saving
      if buffer_modified_p
        message "Buffer has been modified.  Save? (y/n)"
      end

      toggle_read_only 0
      l = Location.new :save_scroll_position => true
      erase_buffer
      insert Xiki.get_page_from_server parse_page_name
      use_local_map elvar.xiki_mode_keymap_read_only
      l.go
      save_buffer
      toggle_read_only 1
    end

  end

  # Follow link, according to key typed
  def self.lowercase_key_typed l
    # If search already in progress, add it
    search = l
    visible = text_visible_on_screen

    found = []

    while true
      message "Searching links beginning with: #{search}"

      # Check for numbers
      if search =~ /([0-9])$/
        return self.blink_and_open_nth found, $1.to_i - 1, visible
      end
      
      found = find_match_indexes search, visible

      case found.size
      when 0
        link_search_over
        message "No links found starting with: #{search}"
        return
      when 1
        return self.blink_and_open_nth found, 0, visible
      else
        link_search_clear_highlights
        search_multiple_links_found found
      end

      # Read in another char
      search += char_to_string read_char_exclusive

    end
  end

  def self.blink_and_open_nth found, n, visible
    one = found[n]
    link_search_over
    self.blink( window_start + one[0], window_start + one[2] )
    self.open_page visible[one[0]..(one[2]-1)]
  end

  def self.number_typed l
    visible = text_visible_on_screen

    found = find_match_indexes "", visible
    self.blink_and_open_nth found, l.to_i - 1, visible

#    links = visible.scan /\[(.+?)\]/
#    if links[l.to_i - 1]
#      Xiki.open_page links[l.to_i - 1][0]
#    else
#      Xiki.open_page links[-1][0]
#    end
  end

  def self.uppercase_key_typed l
    self.search_in_headers l.downcase
  end


  def self.parse_page_name_title_cased
    buffer_file_name.to_s =~ /.*\/(.+)\.xiki$/
    return $1 ? $1.gsub("_", " ").gsub(/\w+/) { $&.capitalize } : nil
  end
  def self.parse_title
    buffer_string =~ /(.*)/
    first_line = $1
    return first_line.sub /.*\//, ""
  end
  def self.parse_server
    buffer_string =~ /(.*)/
    first_line = $1
    first_line =~ /(.+)\//
    return $1 || @@default_server
  end

  def self.parse_page_name_from_file
    buffer_file_name.to_s =~ /.*\/(.+)\.xiki?$/
    return $1 ? $1 : nil
  end

  def self.parse_page_name
    buffer_file_name.to_s =~ /.*\/(.+)\.xiki?$/
    return $1 ? $1 : nil
  end

#  # All pages that open a specific page name go through this
  def self.open_page name, edit=false
    name.downcase!

    name = "home" if name == ""
    self.add_to_history_maybe
    find_file "#{name}.xiki"
    self.add_to_history_maybe
  end

  def self.get_page_from_server name, edit=false
    name.downcase!
    name = "home" if name == ""
    name.gsub! " ", "_"
    server = parse_server
    begin
      params = edit ? {:edit => "true"} : {}
      message "loading..."
      txt = Net::HTTP.post_form(URI.parse("http://#{server}/wiki/show/#{name}"), params).body
      message ""
      clean txt
    rescue Exception => e
      txt = "Error\n- Perhaps server isn't running: #{server}\n- Perhaps a server error occurred\n- Error Message:\n\n #{e}"
    end

#    txt.sub /\A.+\//, ""
    # Add path
    txt.sub /^/, "#{server}/"
#    txt.sub! /^/, "#{@@default_server_root}/"

  end

  def self.clean t
    t.gsub! /\r/, ""
  end


  def self.search_multiple_links_found found
    found.each do |f|
      overlay = make_overlay( f[0] + window_start, f[1] + window_start )
      overlay_put overlay, :face, :xiki_highlight
    end
  end

  def self.link_search_clear_highlights
    overlays = overlays_in(point_min, point_max)
    return unless overlays
    overlays.each do |o|
      if overlay_get(o, :face).to_s == "xiki-highlight"
        delete_overlay o
      end
    end
  end
  def self.link_search_over
    link_search_clear_highlights
  end

  def self.define_global_shortcuts
    Keys.XW do  # Open home page
      Xiki.open_page "home"
    end
  end


  def self.save
    begin

      if elvar.buffer_read_only
        message "Saving can't happen in read-only mode.  Type C-c C-e to go into edit mode."
        return
      end

      message "Saving..."

      # Get server name
      server = parse_server
#      page_name = parse_page_name
#      insert server

      # Pass name and content as yaml?

      # Get rid of server path
      txt = buffer_string.sub /\A.+?\//, ""

      # TODO Server currently pulls title from page
      res = Net::HTTP.post_form(URI.parse("http://#{server}/wiki/save"),
                                { "page[content]" => txt })
      save_buffer

      # TODO check to see if save succeeded
      if res.body =~ /^:ERROR/
        message "Error: #{res.body}", true
      else
        message "Saved successfully #{server}/#{res.body}"
      end

    rescue Exception => e
      wiki_message "Error: #{e}", true
    end

  end

  # Called when user clicks in a link (on other executable line),
  # or presses enter in a link.
  def self.wiki_click typed_enter=false
    line = buffer_substring(point_at_bol, point_at_eol)
    before = buffer_substring(point_at_bol, point)
    after = buffer_substring(point, point_at_eol)

## TODO go back and use LineLauncher?
##  - Pull LineLauncher out of wrapper.el first
    # If the current line has a link, open it
## TODO use LineLauncher to implement these:
#    if (line =~ /- google: /) || (line =~ /http:\/\//)
#      command_execute kbd2("UO")

    # If they clicked in a link, launch it
    if (before =~ /\[/ && after =~ /\]/)
      # Get substring, and open page
      Xiki.open_page before.sub(/.+?([A-Za-z _]*)$/, "\\1") + after.sub(/^([A-Za-z _]*).+/, "\\1")

    end
  end


  def self.define_styles
    el4r_lisp_eval <<-'EOL'
      (progn
        (set-face-attribute (make-face 'xiki-title) nil
          :family "arial black"
          :height 200
          :background "#000044"
          :foreground "#ffffff"
          :box nil
          :box '(:line-width 5 :color "#000044")
          )
        (set-face-attribute (make-face 'xiki-title-url) nil
          :family "arial"
          :weight 'bold
          :foreground "#555588"
          ;:foreground "#333377"
          :background "#000044"
          :box '(:line-width 5 :color "#000044")
          :height 150
          )

;        (set-face-attribute (make-face 'xiki-title-linebreak) nil
;            :family "arial"
;            :weight 'bold
;            :foreground "#555588"
;            ;:foreground "#333377"
;            :background "#000044"
;            :box nil
;            :height 20)

        (set-face-attribute (make-face 'xiki-title-lowcontrast) nil
          :family "arial"
          :weight 'bold
          :foreground "#222255"
          :background "#000044"
          :height 320)
        (set-face-attribute (make-face 'xiki-h1) nil
          :family "arial"
          :height 150
          :background "#666699"
          :foreground "#ffffff"
          ;:box nil
          :weight 'bold)
        (set-face-attribute (make-face 'xiki-h1-beforecolon) nil
          :family "arial"
          :height 150
          :background "#666699"
          :foreground "#bbbbdd"
          :box nil
          :underline nil
          :weight 'bold)
        (set-face-attribute (make-face 'xiki-h1-lowcontrast) nil
          :background "#666699"
          :foreground "#9898cb"
          :family "arial"
          :height 150)
        (set-face-attribute (make-face 'xiki-h2) nil
          :background "#e3e3f3"
          :foreground "#666699"
          :family "verdana"
          :height 120
          :weight 'bold)
        (set-face-attribute (make-face 'xiki-h2-lowcontrast) nil
          :background "#e3e3f3"
          :foreground "#c3c3d3"
          :family "arial"
          :height 130)
        (set-face-attribute (make-face 'xiki-h3) nil
          :foreground "#8888aa"
          :family "verdana"
          :height 110
          :weight 'bold)
        (set-face-attribute (make-face 'xiki-h3-lowcontrast) nil
          :background "#ffffff"
          :foreground "#ddddee"
          :family "verdana"
          :height 110
          )
        (set-face-attribute (make-face 'xiki-dash) nil
          :foreground "#ee7700"
          :family "arial black"
          :height 120
          :weight 'normal)
        (set-face-attribute (make-face 'xiki-dim) nil
          :foreground "#dddddd"
          :height 95)
        ; Link
        (set-face-attribute (make-face 'xiki-link) nil
          :underline t
          :foreground "#3333ee"
        )
        ; Link Brackets
        (set-face-attribute (make-face 'xiki-link-brackets) nil
          :foreground "#cccccc"
        )
        ; Link Params
        (set-face-attribute (make-face 'xiki-link-params) nil
          :foreground "#cccccc"
          :height 90
        )
        ; Highlight when blinking
        (set-face-attribute (make-face 'xiki-highlight) nil
          :foreground "#000000"
          :background "#00ff00"
        )
       )
    EOL

  end
  def self.fontify
    el4r_lisp_eval <<-'EOL'
      (progn
        ; Title
        (font-lock-add-keywords nil '(("\\`\\(.*\n\\)" 
          (1 'xiki-title)
          ;(2 'xiki-title-linebreak)
          )))
        (font-lock-add-keywords nil '(("\\`\\(.+/\\)\\(.*\n\\)" 
          (1 'xiki-title-url)
          (2 'xiki-title)
          ;(3 'xiki-title-linebreak)
          )))

        ; Headings
        (font-lock-add-keywords nil '(("^\\(| \\)\\(.*\n\\)" 
          (1 'xiki-h1-lowcontrast)
          (2 'xiki-h1))))
        (font-lock-add-keywords nil '(("^\\(| \\)\\(.+: \\)\\(.*\n\\)" 
          (1 'xiki-h1-lowcontrast)
          (2 'xiki-h1-beforecolon)
          (3 'xiki-h1))))
        (font-lock-add-keywords nil '(("^\\(|| \\)\\(.*\n\\)" 
          (1 'xiki-h2-lowcontrast)
          (2 'xiki-h2))))
        (font-lock-add-keywords nil '(("^\\(||| \\)\\(.*\n\\)" 
          (1 'xiki-h3-lowcontrast)
          (2 'xiki-h3))))

        (font-lock-add-keywords nil '(("^ *\\(-\\) "
          (1 'xiki-dash))))
        ; Bullets
        (font-lock-add-keywords nil '(("^ *\\(- [a-zA-Z0-9].*?: \\)"
          (1 'xiki-dash))))
        (font-lock-add-keywords nil '(("^ *\\(- [a-zA-Z0-9].*?:\\)$"
          (1 'xiki-dash))))
        ; links
        (font-lock-add-keywords nil '(("\\(\\[\\)\\([a-zA-Z/_\. ]+?\\)\\(\\]\\)"
          (1 'xiki-link-brackets)
          (2 'xiki-link)
          (3 'xiki-link-brackets))))
        (font-lock-add-keywords nil '(("\\(\\[\\)\\([a-zA-Z/_\. ]+?\\)\\([:!].*?\\)\\(\\]\\)"
          (1 'xiki-link-brackets)
          (2 'xiki-link)
          (3 'xiki-link-params)
          (4 'xiki-link-brackets))))
        (font-lock-add-keywords nil '(("\\(~\\)\\(.+?\\)\\(~\\)" 
          (1 'xiki-dim)
          (2 'xiki-dash)
          (3 'xiki-dim))))
      )
    EOL
    font_lock_mode 1
  end
  def self.original_dir
    # Create dirs if not there
    Dir.mkdir(@@xiki_dir) unless File.directory?(@@xiki_dir)
    d = @@original_dir
    Dir.mkdir(d) unless File.directory?(d)
    d
  end

  def self.original_version

    f = buffer_file_name
    f.gsub! /\W/, "_"
    f.sub! /^_/, ""
    f.sub! /_$/, ""
    f.sub! /_xiki$/, ".xiki"

    "#{original_dir}/#{f}"
  end

  def self.save_original_version

#    txt = buffer_string.sub /\|\|\|\| .+\//, "|||| "
    txt = buffer_string

    File.open(original_version, "w") { |f| f << txt }
  end
  def self.hi
    insert "--> xxxx"
  end

  def self.in_orig_dir
    starts_with_dir = "^" + Regexp.escape(original_dir)
    return buffer_file_name =~ /#{starts_with_dir}/
  end

  def self.allow_editing

    if in_orig_dir
      message "Files in this dir shouldn't be modified.  Copy it to another dir and rename it."
      return
    end

    toggle_read_only 0
    l = Location.new :save_scroll_position => true
    erase_buffer
    insert Xiki.get_page_from_server parse_page_name, true
    l.go
    use_local_map elvar.xiki_mode_keymap
    save_buffer

  end
  def self.blink left, right
    over = make_overlay left, right
    (1..2).each do |i|
      overlay_put over, :face, :xiki_highlight
      sit_for 0.05
      overlay_put over, :face, :default
      sit_for 0.05 unless i == 2
    end
    delete_overlay over
  end

  def self.text_visible_on_screen
    visible = buffer_substring(window_start, window_end)
    visible.sub! /.*\n\z/, "" if point_max > window_end
    visible
  end

  def self.search_in_headers search=nil
    Hide.hide_unless /^\| /, :include_first => true
    recenter -3
    Hide.search search, :include_first => true, :expand_when_one => true
  end

  def self.back
    @@history.pop
    if @@history.empty?
      message "You've gone all the way back to your starting point"
      return
    end
    find_file @@history[-1]
  end

  def self.find_match_indexes search, visible
    found = []
    s = StringScanner.new(visible)
    while s.scan /.*?\[#{search}(.*?)\]/mi
      found << [s.pos - (s[1].size + 1 + search.size), s.pos - (s[1].size + 1), (s.pos - 1)]
    end
    found
  end

  def self.add_to_history_maybe
    return unless buffer_file_name =~ /\.xiki?$/
    return if @@history[-1] == buffer_file_name
    @@history << buffer_file_name
  end

end
Xiki.startup

