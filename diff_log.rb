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

  # Reused by EIO and EIN
  def self.old_or_new_diff &closure
    diff = get_register ?D
    # Get rid of path lines
    diff.gsub! /^\/.+\n/, ""
    # Delegate to closure
    closure.call(diff)
    insert diff
  end

  # Insert old text deleted during last save
  def self.enter_old
    self.old_or_new_diff do |diff|
      # Get rid of +... lines
      diff.gsub! /^\+.*\n/, ""
      # Remove - from -... lines
      diff.gsub! /^-/, ""
    end
  end

  # Insert new text added during last save
  def self.enter_new
    self.old_or_new_diff do |diff|
      diff
      # Get rid of -... lines
      diff.gsub! /^-.*\n/, ""
      # Remove + from +... lines
      diff.gsub! /^\+/, ""
    end
  end

  # Appends diff to difflog, then saves.  Map to AF.
  def self.save
    unless View.file_name == "difflog.notes"
      diff = self.saved_diff
      # Write to temporary file
      File.open(@@log, "a") { |f| f << diff } unless diff.count("\n") <= 2
    end
    save_buffer

    if Keys.prefix_u?
      sleep(0.3)
      Firefox.reload
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
    "#{path}\n" +
    "  #{file}\n" +
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

private
  # Util function used by public functions
  def self.saved_diff
    $el.write_region View.top, View.bottom, @@temp_path
    diff = Console.run "diff -w -U 0 \"#{buffer_file_name}\" \"#{@@temp_path}\"", :sync=>true
    self.format(View.path, View.file_name, diff)
  end
end
