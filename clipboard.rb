require 'keys'
require 'file_tree'

# Provides copy and paste functionality
class Clipboard
  extend ElMixin

  # Stores things user copies
  @@hash = {}
  @@hash_by_first_letter = {}

  def self.menu
    "
    - .log/
    "
  end

  def self.log
    result = ""
    @@hash_by_first_letter.keys.sort.each do |k|
      result << "| #{@@hash_by_first_letter[k]}\n"
    end
    result.empty? ? "- Nothing was copied yet!" : result
  end

  def self.copy loc=nil, txt=nil
    # Use string if user types it quickly
    loc ||= Keys.input(:one_char=>true, :prompt=>"Enter one char (to store this as): ") || "0"

    unless txt
      left, right = View.range
      Effects.blink :left=>left, :right=>right
      txt = buffer_substring(region_beginning, region_end)
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
    delete_region(region_beginning, region_end)

    Location.as_spot('killed')
  end

  def self.paste loc=nil
    # Use string if user types it quickly
    loc ||= Keys.input(:one_char => true, :prompt => "Enter one char: ") || 0

    $el.set_mark_command nil

    loc = loc.to_s
    txt = @@hash[loc] || @@hash_by_first_letter[loc]   # If nothing, try to grab by first letter

    # If nothing, try to grab from what's been searched
    txt ||= Search.searches.find{|o| o =~ /^#{loc}/i}

    return View.message("Nothing to search for matching '#{loc}'.", :beep=>1) if txt.nil?

    (elvar.current_prefix_arg || 1).times do   # Get from corresponding register
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
    switch_to_buffer "*clipboard*"
    erase_buffer
    notes_mode

    Clipboard.hash.sort.each do |a, b|
      insert "| #{a}\n#{b}\n\n"
    end
    beginning_of_buffer
  end

  def self.set loc, str, append=nil
    loc = loc.to_s
    # Save in corresponding register (or append if prefix)
    if append
      @@hash[loc] += str
    else
      # Store as path
      @@hash["/"] = expand_file_name( buffer_file_name ? buffer_file_name : elvar.default_directory )
      if buffer_file_name
        # Store as tree snippet
        @@hash["="] = FileTree.snippet(str)
        @@hash["."] = "#{file_name_nondirectory(buffer_file_name)}"
        @@hash["\\"] = "#{elvar.default_directory}\n  #{file_name_nondirectory(buffer_file_name)}"
      end
      @@hash[loc] = str
      x_select_text str if loc == "0"  # If 0, store in OS clipboard
    end
  end

  def self.do_as_snake_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      insert TextUtil.snake_case(word)
      Move.forward
    end
  end

  def self.do_as_camel_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      insert TextUtil.camel_case(word)
      Move.forward
    end
  end

  def self.do_as_upper_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      insert word.upcase
      Move.forward
    end
  end

  def self.do_as_lower_case
    Keys.prefix_times.times do
      word = Line.symbol(:delete => true)
      insert word.downcase
      Move.forward
    end
  end

  def self.copy_paragraph options={}
    if Keys.prefix_u? or options[:rest]   # If U prefix, get rest of paragraph
      left, right = View.paragraph(:bounds => true, :start_here => true)
    else
      if Keys.prefix   # If numeric prefix
        self.as_line
        return
      end
      # If no prefix, get whole paragraph
      left, right = View.paragraph(:bounds => true)
    end

    if options[:just_return]
      return [View.txt(left, right), left, right]
    end
    goto_char left
    set_mark right
    Effects.blink(:left => left, :right => right)
    Keys.clear_prefix
    Clipboard.copy("0")
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

    ediff_buffers "1", "2"
  end

  def self.as_thing

    # If on blank spaces, use them
    if buffer_substring(point-1, point+1) =~ /[ \n] /
      skip_chars_forward " "
      right = point
      skip_chars_backward " "
      left = point
    else
      skip_chars_forward " "
      left, right = bounds_of_thing_at_point(:sexp).to_a
    end

    if Keys.prefix_u?
      left += 1
      right -= 1
    end
    Effects.blink(:left=>left, :right=>right)

    txt = View.txt(left, right)
    Clipboard.set "0", txt
    View.to right
    View.mark left
    Clipboard.save_by_first_letter txt
  end

  def self.as_object
    set("0", thing_at_point(:symbol))
    left, right = bounds_of_thing_at_point(:symbol).to_a
    Effects.blink(:left=>left, :right=>right)
  end

  def self.copy_everything
    Effects.blink :what=>:all
    Clipboard.set("0", buffer_string)
    set_mark(point_max)
  end

  def self.as_line many=nil
    many ||= Keys.prefix(:clear=>true) || 1
    Move.to_axis
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
    skip_chars_forward " "

    orig = point
    Move.to_other_bracket
    View.delete orig, point
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
    ch = Keys.input :one_char => true
    value = @@hash_by_first_letter[ch]
    return unless value
    View.insert value
  end

end
