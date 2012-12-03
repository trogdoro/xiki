require 'xiki/hide'

# Will store a diff each time a file is saved.
class DiffLog

  @@log = File.expand_path("~/.emacs.d/difflog.notes")
  @@temp_path = "/tmp/saved.txt"

  def self.menu
    "
    .open/
    .diffs/
    .diffs/$p/
    docs/
      > Summary
      | The difflog tracks diffs of all the changes you make to file
      | (assuming you save using as+file).
      |
      > Keys
      | open+diffs - open the difflog
      | search+diffs - search the difflog from the bottom
    "
  end

  # Open file having difflog
  def self.open
    View.to_after_bar if View.in_bar?

    # If open, just switch to it and revert
    if View.buffer_open?("difflog.notes")
      View.to_buffer("difflog.notes")
      $el.revert_buffer true, true, true
    else  # Otherwise, open it
      View.open(@@log)
    end
    View.to_bottom
    Line.previous
    Line.to_words
    $el.recenter -4
  end

  def self.diffs path=nil

    txt = File.open(@@log, 'rb') {|f| f.read}
    txt = txt.sub(/\A- /, '').split(/^- /).reverse.uniq

    path ||= Tree.dir   # Pull from tree if there

    path = Bookmarks[path] if path

    if ! path
      regex = /^\//
    elsif File.file? path   # File
      regex = /^#{Regexp.escape File.dirname path}\/\n  - #{Regexp.escape File.basename path}/
    else   # Dir
      regex = /^#{Regexp.escape path}/
      path = "#{path}/" if path !~ /\/$/
    end

    txt = txt.select{|o| o =~ regex}

    "- @#{txt.join("- @")}"
  end

  # Insert old text deleted during last save
  def self.last_diff
    $el.with(:save_window_excursion) do
      DiffLog.open
      Search.backward "^-"
      txt = View.txt View.cursor, View.bottom
    end
  end

  def self.is_one_line_change? txt
    txt.scan(/^ +\|\-/).length == 1 &&
      txt.scan(/^ +\|\+/).length == 1 &&
      txt.scan(/^ +:/).length
  end

  # Insert new text added during last save
  def self.enter_new
    self.enter_old_or_new :new
  end

  def self.enter_old
    self.enter_old_or_new :old
  end

  def self.enter_old_or_new old_or_new

    diff = DiffLog.last_diff
    one_line_change = DiffLog.is_one_line_change? diff

    # Show intraline change if changed just one line and not up+

    if ! Keys.up? && one_line_change
      View.<< DiffLog.last_intraline_diff[old_or_new == :old ? 0 : 1] rescue View.beep $!
      return
    end

    # Show lines

    diff.gsub! /^ *[+:-].*\n/, ""   # Only leave red and green lines
    if old_or_new == :old
      diff.gsub! /^ +\|\+.*\n/, ""
      diff.gsub! /^ +\|\-/, ""
    else
      diff.gsub! /^ +\|\-.*\n/, ""
      diff.gsub! /^ +\|\+/, ""
    end
    View << diff

  end

  # Appends diff to difflog, then saves.  Mapped to as_file.
  def self.save
    return if View.file_name == "difflog.notes"

    prefix = Keys.prefix :clear=>1

    self.save_diffs

    $el.save_buffer

    if prefix == :u
      sleep(0.3)
      Firefox.reload
    elsif prefix == 9
      Launcher.do_last_launch
    end
  end

  def self.format raw, options={}
    if options[:use_other_path]
      path, file = raw.match(/\+\+\+ (.+\/)(.+?)\t/)[1..2]
    else
      path, file = raw.match(/--- (.+\/)(.+?)\t/)[1..2]
    end

    # Delete paths at top
    raw.sub!(/.+\n.+\n/, '')

    # Make @@... lines into lines having numbers
    raw.gsub!(/^@@ -(\d+).* \+(\d+).+@@$/) {
      a, b = $1.to_i, $2.to_i
      highest = a > b ? a : b
      "    :#{highest}"
    }

    # Make - and + lines into -| and +| lines
    raw.gsub!(/^\+(.*)/, "      |+\\1")
    raw.gsub!(/^-(.*)/, "      |-\\1")

    # Return with path
    "- #{path}\n" +
    "  - #{file}\n" +
    raw
  end

  def self.compare_with_saved

    if Keys.prefix_u

      buffer_unsaved = View.buffer

      path = View.file

      View.to_buffer "untitled"
      $el.rename_uniquely
      View << File.read(path)

      return $el.ediff_buffers View.buffer, buffer_unsaved

    end

    diff = self.save_diffs :dont_log=>1
    diff = "" if diff.nil?

    View.to_buffer("*diff with saved*")
    View.clear
    Notes.mode

    View.insert diff.count("\n") > 2 ?
      diff :
      "> Note\n- No Differences!\n"
  end

  def self.enter_from_difflog
    Location.as_spot
    View.to_after_bar if View.in_bar?
    DiffLog.open
    $el.isearch_backward
  end

  # Util function used by public functions
  def self.save_diffs options={}
    if options[:patha] && options[:textb]
      patha = options[:patha]
      File.open(@@temp_path, "w") { |f| f << options[:textb] }
    else
      patha = View.file
      $el.write_region nil, nil, @@temp_path
    end

    diff = Console.sync "diff -U 0 \"#{patha}\" \"#{@@temp_path}\""
    return if diff.empty? || diff =~ /: No such file or directory\n/   # Fail quietly if file didn't exist

    diff = self.format(diff) rescue nil
    return diff if diff.nil? || options[:dont_log]

    File.open(@@log, "a") { |f| f << diff } unless diff.count("\n") <= 2
  end

  def self.parse_tree_diffs txt
    result = [[], []]

    txt = txt.split("\n")
    i, length = 0, txt.length

    while i < length
      line = txt[i]
      #       a_or_d = line[/^[ad]/]
      match = line.match(/^(.)(\d+) (\d+)/)
      raise "Line #{line} unexpected by DiffLog.parse_tree_diffs" if ! match
      a_or_d, line_number, many = match[1..3]
      line_number, many = line_number.to_i, many.to_i

      if a_or_d == "d"
        i += 1
        many.times do |n|
          result[1] << line_number + n
        end
        next
      end

      if a_or_d == "a"
        i += 1
        many.times do |n|
          result[0] << line_number + n
        end
        next
      end

      raise "DiffLog.parse_tree_diffs doesn't know what to do with #{line}"
    end

    result
  end

  def self.do_compare_with
    prefix = Keys.prefix :clear=>1

    # up+ means compare buffers in first two views

    return $el.ediff_buffers( $el.window_buffer($el.nth(0, $el.window_list)), $el.window_buffer($el.nth(1, $el.window_list))) if prefix == :u

    # Save place and grab file at spot

    source_path = Tree.dir_at_spot
    dest_path = Tree.dir :file=>1
    $el.ediff_files source_path, dest_path

  end

  #
  # Grabs last diff from difflog, and calculates exactly what changed.  Whithin the line.
  # So, it performs a single-line diff
  #
  # It presumes a single-line change, and a change at only one point on the line.
  #
  # DiffLog.last_intraline_diff
  #
  def self.last_intraline_diff txt=nil
    txt ||= DiffLog.last_diff

    linea = txt[/ +\|\-(.+)/, 1]
    lineb = txt[/ +\|\+(.+)/, 1]

    raise "The last diff in the difflog didn't have a red and a green." if linea.nil? || lineb.nil?

    linea_length, lineb_length = linea.length, lineb.length

    from_start = 0
    while from_start < linea_length
      break if linea[from_start] != lineb[from_start]
      from_start += 1
    end

    from_end = 1
    while from_end < linea_length
      break if from_end > linea_length || from_end > lineb_length || linea[linea_length-from_end] != lineb[lineb_length-from_end]
      from_end += 1
    end

    deltaa = from_end > linea_length ? "" : linea[from_start..(linea_length-from_end)]
    delteab = from_end > lineb_length ? "" : lineb[from_start..(lineb_length-from_end)]

    [deltaa, delteab]
  end

end
