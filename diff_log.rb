require 'hide'

# Will store a diff each time a file is saved.
class DiffLog
  extend ElMixin

  @@log = File.expand_path("~/.emacs.d/difflog.notes")
  @@temp_path = elvar.temporary_file_directory + "latest-diff.txt"

  # Open file having difflog
  def self.open
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

    if path == nil
      match_tree = "^- "
    else
      path = Bookmarks[path]
      match_tree = File.directory?(path) ?   # If it's a dir
        "^- #{path}" :
        "^- #{File.dirname path}/\n  - #{File.basename path}\n"
    end

    txt = File.read @@log
    txt = txt.sub(/\A- /, '').split(/^- /).reverse.uniq

    if File.file? path   # File
      regex = /^#{Regexp.escape File.dirname path}\/\n  - #{Regexp.escape File.basename path}/
    else   # Dir
      regex = /^#{Regexp.escape path}/
      path = "#{path}/" if path !~ /\/$/
    end

    txt = txt.select{|o| o =~ regex}

    "- #{txt.join("- ")}"
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

  def self.save_internal options={}
    return if View.file_name == "difflog.notes"
    diff = self.saved_diff options
    File.open(@@log, "a") { |f| f << diff } unless diff.count("\n") <= 2
  end

  # Appends diff to difflog, then saves.  Map to AF.
  def self.save
    self.save_internal
    save_buffer

    prefix = Keys.prefix
    if prefix == :u
      sleep(0.3)
      Firefox.reload
    elsif prefix == 9
      sleep(0.3)
      Launcher.do_last_launch
    end
  end

  def self.format path, file, raw
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
    diff = self.saved_diff
    View.to_buffer("*diff with saved*")
    View.clear
    notes_mode

    insert diff.count("\n") > 2 ?
      diff :
      "| Alert\n- ~No Differences~\n"
  end

  def self.enter_from_difflog
    Location.as_spot
    View.to_after_bar if View.in_bar?
    DiffLog.open
    isearch_backward
  end

  # Util function used by public functions
  def self.saved_diff options={}

    if options[:patha] && options[:textb]
      patha = options[:patha]
      File.open(@@temp_path, "w") { |f| f << options[:textb] }
    else
      patha = View.file
      $el.write_region nil, nil, @@temp_path
    end

    diff = Console.run "diff -w -U 0 \"#{patha}\" \"#{@@temp_path}\"", :sync=>true
    self.format(View.path, View.file_name, diff)
  end
end
