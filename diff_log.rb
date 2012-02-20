require 'hide'

# Will store a diff each time a file is saved.
class DiffLog
  extend ElMixin

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
      revert_buffer true, true, true
    else  # Otherwise, open it
      View.open(@@log)
    end
    View.to_bottom
    Line.previous
    Line.to_words
    recenter -4
  end

  def self.diffs path=nil

    txt = File.read @@log
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
    with(:save_window_excursion) do
      DiffLog.open
      Search.backward "^-"
      txt = View.txt View.cursor, View.bottom
    end
  end

  def self.enter_old
    diff = DiffLog.last_diff
    diff.gsub! /^ *[+:-].*\n/, ""   # Only leave red and green lines

    diff.gsub! /^ +\|\+.*\n/, ""
    diff.gsub! /^ +\|\-/, ""

    View.insert diff
  end

  # Insert new text added during last save
  def self.enter_new
    diff = DiffLog.last_diff
    diff.gsub! /^ *[+:-].*\n/, ""   # Only leave red and green lines

    diff.gsub! /^ +\|\-.*\n/, ""
    diff.gsub! /^ +\|\+/, ""

    View.insert diff
  end

  # Appends diff to difflog, then saves.  Mapped to as_file.
  def self.save
    return if View.file_name == "difflog.notes"
    self.save_diffs

    $el.save_buffer

    prefix = Keys.prefix
    if prefix == :u
      sleep(0.3)
      Firefox.reload
    elsif prefix == 9
      Launcher.do_last_launch
    end
  end

  def self.format raw
    path, file = raw.match(/--- (.+\/)(.+?)\t/)[1..2]

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
    diff = self.save_diffs :dont_log=>1
    diff = "" if diff.nil?

    View.to_buffer("*diff with saved*")
    View.clear
    $el.notes_mode

    View.insert diff.count("\n") > 2 ?
      diff :
      "> Note\n- No Differences!\n"
  end

  def self.enter_from_difflog
    Location.as_spot
    View.to_after_bar if View.in_bar?
    DiffLog.open
    isearch_backward
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

    diff = Console.sync "diff -w -U 0 \"#{patha}\" \"#{@@temp_path}\""
    return if diff.empty? || diff =~ /: No such file or directory\n/   # Fail quietly if file didn't exist

    diff = self.format diff
    return diff if options[:dont_log]

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

end
