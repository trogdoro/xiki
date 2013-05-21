require 'xiki/core/keys'
require 'xiki/core/file_tree'

# Provides copy and paste functionality
class Clipboard

  # Stores things user copies
  @@hash ||= {}
  @@hash_by_first_letter ||= {}

  def self.menu
    %`
    - .log/
    - docs/
      - main clipboard/
        | The "0" clipboard is where things are stored when you do these
        | shortcuts:
        - as+clipboard
        - search+copy

        | (They're also put in the OS clipboard for convenience.)
      - numbered clipboards/
        | You can type these shortcuts use the numbered clipboards
        - as+1
        - as+2
        - enter+2
        - do+1   # Does search and replace from the "1" to "2" clipboard
      - other clipboards/
        | You can type these shortcuts use other letters
        - as+variable+a   # Stores in "a"
        - as+variable+b
        - enter+variable+b
        - search+like+variable+b
    - see/
      <@ replace/
    `
  end

  def self.log key=nil

    # /log/, so show keys...

    if ! key
      result = ""
      keys = @@hash.keys.sort! {|a, b| a.sub(/[!-\/]/, '~') <=> b.sub(/[!-\/]/, '~') }   # Move numbers to top
      keys.each do |k|
        val = Tree.quote @@hash[k]
        val.gsub! /^/, '  '
        result << "| #{k}\n"
      end
      return result.empty? ? "- Nothing was copied yet!" : result
    end

    # /log/foo, so show value...

    line = Line.value.sub(/^ *\| /, "")

    Tree.quote @@hash[line]
  end

  def self.copy loc=nil, txt=nil
    # Use string if user types it quickly
    if ! loc
      View.flash "Enter variable name:", :times=>1
      loc = Keys.input(:chars=>1, :prompt=>"Enter one char (variable name to store this as): ") || "0"
    end

    unless txt
      left, right = View.range
      Effects.blink :left=>left, :right=>right
      txt = $el.buffer_substring($el.region_beginning, $el.region_end)
    end
    self.set(loc, txt, Keys.prefix)
  end

  def self.cut loc=nil
    loc = loc.to_s
    prefix = Keys.prefix :clear=>true   # If numeric prefix, reset region

    if prefix == 0
      l, r = View.paragraph :bounds=>true
      View.cursor = l
      View.mark = r
    elsif prefix.is_a?(Fixnum)
      Line.to_left
      View.mark = Line.left 1+prefix
    end

    self.copy loc
    $el.delete_region($el.region_beginning, $el.region_end)

    Location.as_spot('killed')
  end

  def self.paste loc=nil
    # Use string if user types it quickly
    loc ||= Keys.input(:chars=>1, :prompt => "Enter one char: ") || 0

    $el.set_mark_command nil

    loc = loc.to_s
    txt = @@hash[loc] || @@hash_by_first_letter[loc]   # If nothing, try to grab by first letter

    # If nothing, try to grab from what's been searched
    txt ||= Search.searches.find{|o| o =~ /^#{loc}/i}

    return View.message("Nothing to search for matching '#{loc}'.", :beep=>1) if txt.nil?

    ($el.elvar.current_prefix_arg || 1).times do   # Get from corresponding register
      View << txt
    end
  end

  def self.get key='0', options={}
    val = @@hash[key.to_s]
    if options[:add_linebreak]
      val = "#{val}\n" unless val[/\n$/]
    end
    val
  end

  def self.[] key
    self.get key.to_s
  end

  def self.[]= key, to
    self.set key.to_s, to
  end

  def self.display
    @@hash.each do |k, v|
      insert [k, v].to_s
    end
  end

  def self.hash
    @@hash
  end

  def self.hash_by_first_letter
    @@hash_by_first_letter
  end

  def self.list
    $el.switch_to_buffer "*clipboard*"
    $el.erase_buffer
    Notes.mode

    Clipboard.hash.sort.each do |a, b|
      $el.insert "| #{a}\n#{b}\n\n"
    end
    $el.beginning_of_buffer
  end

  def self.set loc, str, append=nil
    loc = loc.to_s
    # Save in corresponding register (or append if prefix)
    if append
      @@hash[loc] += str
    else
      # Store as path
      @@hash["/"] = $el.expand_file_name( $el.buffer_file_name ? $el.buffer_file_name : $el.elvar.default_directory )
      if $el.buffer_file_name
        # Store as tree snippet
        @@hash["="] = FileTree.snippet :txt=>str
        @@hash["."] = "#{$el.file_name_nondirectory($el.buffer_file_name)}"
        @@hash["\\"] = "#{$el.elvar.default_directory}\n  #{$el.file_name_nondirectory($el.buffer_file_name)}"
      end
      @@hash[loc] = str
      $el.x_select_text str if loc == "0"  # If 0, store in OS clipboard
    end
  end

  def self.do_as_snake_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      $el.insert TextUtil.snake_case(word)
      Move.forward
    end
  end

  def self.do_as_camel_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      $el.insert TextUtil.camel_case(word)
      Move.forward
    end
  end

  def self.do_as_upper_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      $el.insert word.upcase
      Move.forward
    end
  end

  def self.do_as_lower_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      $el.insert word.downcase
      Move.forward
    end
  end

  def self.copy_paragraph options={}
    prefix = Keys.prefix

    if prefix == :u or options[:rest]   # If U prefix, get rest of paragraph
      left, right = View.paragraph(:bounds => true, :start_here => true)
    else
      if prefix   # If numeric prefix
        self.as_line
        return
      end
      # If no prefix, get whole paragraph
      left, right = View.paragraph(:bounds => true)
    end

    if options[:just_return]
      return [View.txt(left, right), left, right]
    end
    $el.goto_char left
    $el.set_mark right
    Effects.blink(:left => left, :right => right)
    Clipboard.copy("0")
  end

  def self.copy_name
    Clipboard.set("0", Files.stem)
  end

  def self.diff_1_and_2
    # Compare clipboard 1 with 2
    # Unquote if several spaces and |
    View.to_buffer "1", :clear => true
    one = Clipboard["1"]
    one.gsub!(/^ +\|/, '') if one =~ /\A   +\|/
    $el.insert Clipboard["1"]

    View.to_buffer "2", :clear => true
    one = Clipboard["2"]
    one.gsub!(/^ +\|/, '') if one =~ /\A   +\|/
    $el.insert Clipboard["2"]

    $el.ediff_buffers "1", "2"
  end

  def self.as_thing

    orig = Location.new

    # If at end of space, grab as tree
    if Line.indent.length == View.column
      left = Line.left
      return
    end

    # If on blank spaces, copy them
    if $el.buffer_substring($el.point-1, $el.point+1) =~ /[ \n] /
      $el.skip_chars_forward " "
      right = $el.point
      $el.skip_chars_backward " "
      left = $el.point
    else
      $el.skip_chars_forward " "
      left, right = $el.bounds_of_thing_at_point(:sexp).to_a
    end

    if Keys.prefix_u?
      left += 1
      right -= 1
    end
    Effects.blink(:left=>left, :right=>right)

    txt = View.txt(left, right)
    Clipboard.set "0", txt
    View.to right
    #     View.mark left   # What did this do?
    Clipboard.save_by_first_letter txt

    orig.go

  end

  def self.as_object
    set("0", $el.thing_at_point(:symbol))
    left, right = $el.bounds_of_thing_at_point(:symbol).to_a
    Effects.blink(:left=>left, :right=>right)
  end

  def self.copy_everything
    Effects.blink :what=>:all
    Clipboard.set("0", $el.buffer_string)
    $el.set_mark($el.point_max)
  end

  def self.as_line many=nil
    prefix = Keys.prefix :clear=>true

    # If Dash+, copy Foo.bar from quoted line
    if prefix == :-
      txt = Ruby.quote_to_method_invocation
      View.flash "- copied #{txt}"
      return Clipboard.set("0", txt)
    end

    # If up+, copy path of file tree cursor is on, or otherwise the current view's
    return FileTree.copy_path if prefix == :u

    many ||= prefix || 1
    left = Line.left
    right = Line.left(many+1)
    line = View.txt(left, right)
    Clipboard.set("0", line)
    Effects.blink :left=>left, :right=>right
    $el.set_mark(right)
    Clipboard.save_by_first_letter line   # Store for retrieval with enter_yank
  end

  def self.enter_replacement
    # If on whitespace, move to off of it
    $el.skip_chars_forward " "

    orig = $el.point
    Move.to_other_bracket
    View.delete orig, $el.point
    View.insert Clipboard['0']
  end

  def self.as_clipboard
    prefix = Keys.prefix :clear=>true
    if prefix == 0
      l, r = View.paragraph :bounds=>true
      Effects.blink :left=>l, :right=>r
      cursor = View.cursor
      View.cursor = l
      Location.as_spot('clipboard')
      Clipboard["0"] = View.txt(l, r)
      View.cursor = cursor
      return
    end

    if prefix == :-
      l, r = View.range
      Effects.blink :left=>l, :right=>r
      Clipboard["0"] = View.selection.gsub(/^ *\|.?/, '')
      return
    end

    Location.as_spot('clipboard')

    # If numeric prefix, get next n lines and put in clipboard
    if prefix.is_a?(Fixnum)
      l, r = Line.left, Line.left(prefix + 1)
      Effects.blink :left=>l, :right=>r
      Clipboard["0"] = View.txt(l, r)

      View.set_mark(r)
      return
    end

    Clipboard.copy("0")
    Clipboard.save_by_first_letter View.selection   # Store for retrieval with enter_yank
  end

  def self.save_by_first_letter txt
    key = txt[/[a-z]/i]
    return unless key
    @@hash_by_first_letter[key.downcase] = txt
  end

  def self.enter_yank
    ch = Keys.input :chars=>1
    value = @@hash_by_first_letter[ch]
    return unless value
    View.insert value
  end

end
