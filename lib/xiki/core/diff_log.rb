require 'xiki/core/hide'
require 'digest/md5'

module Xiki
  # Will store a diff each time a file is saved.
  class DiffLog

    @@log = File.expand_path("~/.xiki/misc/logs/difflog.xiki")
    @@temp_path = "/tmp/saved.txt"

    def self.menu
      "
      .open/
      .diffs/
      .diffs/:p/
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
      if View.buffer_open?("difflog.xiki")
        View.to_buffer("difflog.xiki")
        $el.revert_buffer true, true, true
      else  # Otherwise, open it
        View.open(@@log)
      end
      View.to_bottom
      Line.previous
      Line.to_words
      $el.recenter -4
    end


    # Called by =edits
    def self.diffs path=nil

      txt = File.open(@@log, 'rb') {|f| f.read}

      # Add artificial "|" delimiter, to make for easier splitting
      txt.gsub! /(^[\/~])/, "|\\1"

      # Split it by "|" at beginning of lines
      txt = txt.split(/^\|/)
      txt.slice! 0
      txt = txt.reverse.uniq
      path ||= Tree.dir   # Pull from tree if there

      # Temp fix > remove slash from path (Tree.dir is adding slashes whet it's a file in the path)
      # Longer-term fix is to make Tree.dir not add slash when quotes
      path.sub! /\/$/, ''


      path = Bookmarks[path] if path

      if ! path
        regex = /^\//
      elsif File.file? path   # File
        path = Files.tilde_for_home path
        regex = /^#{Regexp.escape File.dirname path}\/\n  - #{Regexp.escape File.basename path}/
      else   # Dir
        path = Files.tilde_for_home path
        regex = /^#{Regexp.escape path}/
        # Was doing nothing
        #         path = "#{path}/" if path !~ /\/$/
      end
      txt = txt.select{|o| o =~ regex}
      "= #{txt.join("= ")}"
    end

    # Insert old text deleted during last save
    def self.last_diff
      $el.with(:save_window_excursion) do
        DiffLog.open
        Search.backward "^[^ \n]"
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

      diff = self.last_diff
      one_line_change = DiffLog.is_one_line_change? diff

      # Show intraline change if changed just one line and not up+

      if ! Keys.up? && one_line_change
        View.<< DiffLog.last_intraline_diff[old_or_new == :old ? 0 : 1] rescue View.beep $!
        return
      end

      # Show lines

      if old_or_new == :old
        diff = diff.scan(/^ +:-(.+)/).join("\n")
      else
        diff = diff.scan(/^ +:\+(.+)/).join("\n")
      end

      View.<< "#{diff}\n", :dont_move=>1

    end

    # Appends diff to difflog, then saves.  Mapped to as_file.
    def self.save_newly_created

      Move.top
      View >> "\n\n"
      View << "* save\n  | #{Notes.share_or_save_prompt_text}\n  : "

    end

    def self.save options={}

      file = View.file

      if View.name == "diff with saved"   # If viewing diff, close it and save the actual file...
        file = View.txt[/.+\n.+/].sub("\n  - ", "")
        View.kill
        View.open file
      end
      prefix = Keys.prefix :clear=>1

      # It's unsaved and not associated with a file yet, so prompt for where to save it
      return self.save_newly_created if ! file

      diffs = self.save_diffs if prefix != :- && prefix != :u && ! options[:no_diffs]   #> |||||

      View.message ""
      $el.save_buffer
      View.message ""

      # Save to xikihub if shared stuff was edited...

      XikihubClient.save file, options

      # If there's a ~/xiki/foo.link for this file, update its timestamp
      # as well (so it appears at the top of list+topics).
      Notes.update_link_timestamp_if_any file

      View.message ""

    end

    def self.format raw, options={}

      if options[:use_other_path].is_a? String
        match = options[:use_other_path].match(/(.+\/)(.+)/)
        path, file = match[1..2]
      elsif options[:use_other_path]
        match = raw.match(/\+\+\+ (.+\/)(.+?)\t/)
        path, file = match[1..2]
      else
        match = raw.match(/--- (.+\/)(.+?)\t/)
        raise "| No ---... line in the diff.\n| The diff command that made the input should have the flags: -U 0" if ! match
        path, file = match[1..2]
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
      raw.gsub!(/^\+/, "      :+")
      raw.gsub!(/^-/, "      :-")
      raw.gsub!(/^\\ (.+)/, "      : (\\1)")

      path = Files.tilde_for_home path

      if options[:children_only]
        return raw.gsub(/^    :.+\n/, '')
      end

      if options[:diffs_only]
        return raw.gsub(/^    /, '')
      end

      # Return with path
      "#{path}\n" +
      "  - #{file}\n" +
      raw
    end


    def self.diff_strings string1, string2, options={}

      path1, path2 = "/tmp/diff1", "/tmp/diff2"
      File.open(path1, "w") { |f| f << "#{string1.strip}\n" }
      File.open(path2, "w") { |f| f << "#{string2.strip}\n" }
      txt = Shell.sync "diff -U 0 \"#{path1}\" \"#{path2}\""
      return txt if options[:raw]

      self.format txt, options

    end


    def self.compare_with_saved

      # up+up+, so do side-by-side...

      if Keys.prefix_u
        return Launcher.open("unsaved", :name=>"unsaved changes")
      end

      if Keys.prefix_uu

        buffer_unsaved = View.buffer

        path = View.file

        View.to_buffer "untitled"
        $el.rename_uniquely
        View << File.read(path)

        return $el.ediff_buffers View.buffer, buffer_unsaved

      end

      diff = self.save_diffs :just_return=>1   #> |||||
      View.message ""   # To avoid message when writing
      diff = "" if diff.nil?
      no_difference = diff.count("\n") <= 2

      if no_difference
        $el.set_buffer_modified_p nil
      end


      View.to_buffer("diff with saved")
      View.clear
      Notes.mode :wrap=>false
      # View.wrap :off

      # No differences, so just say so...

      if no_difference #diff.count("\n") <= 2
        View.insert("- No unsaved changes!\n\nclose view/\n")
        Line.previous 1
      else

        View.insert diff + "\nsave/\nsave without diff/\nrevert/\n"
        Line.previous 3
      end
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
        View.message ""
      end

      # New file, so show all as new...
      diff =
        if patha

          # Use blank if file doesn't exist
          patha = "/dev/null" if ! File.exists?(patha)

          txt = Shell.sync "diff -U 0 \"#{patha}\" \"#{@@temp_path}\""   #> ||||||
          txt_orig = txt.dup

          return if txt.empty? || txt =~ /: No such file or directory\n/   # Fail gracefully if file didn't exist
          self.format(txt) rescue nil
        else
          # Wasn't a buffer rather than a file, so show all as new
          txt = View.txt
          txt.gsub! /^/, "      :+"
          "#{View.dir}/\n  - #{View.name}\n    :1\n#{txt}\n\n"
        end

      return diff if ! diff || options[:just_return]

      FileUtils.mkdir_p File.dirname(@@log)
      File.open(@@log, "a") { |f| f << diff } unless diff.count("\n") <= 2

      txt_orig
    end

    def self.parse_tree_diffs txt
      result = [[], []]

      txt = txt.split("\n")
      i, length = 0, txt.length

      while i < length
        line = txt[i]
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

    def self.compare_in_tree

      prefix ||= Keys.prefix :clear=>1

      source_path = Tree.file_at_spot
      dest_path = Tree.dir :file=>1

      FileTree.extract_filters! dest_path   # Remove ##.../ parts of path

      [source_path, dest_path].each {|o| raise ": File doesn't exist: #{o}" if ! File.exists? o }

      # up+, so ediff...

      return $el.ediff_files(source_path, dest_path) if prefix == :u

      # Xiki diff in one view...

      self.diff_files source_path, dest_path, :display_in_view=>1

    end

    def self.compare_views

      # Dash+, so compare buffers in first two views...

      prefix ||= Keys.prefix :clear=>1

      source_buffer = $el.window_buffer($el.nth(0, $el.window_list))
      dest_buffer = $el.window_buffer($el.nth(1, $el.window_list))

      # up+, so ediff...

      return $el.ediff_buffers(source_buffer, dest_buffer) if prefix == :u

      # Xiki diff in one view...

      DiffLog.diff_files View.as_file_or_temp_file, View.as_file_or_temp_file(:buffer=>dest_path), :display_in_view=>1

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

    def self.diff_files source_path, dest_path, options={}

      display_in_view = options[:display_in_view]

      txt = Shell.sync "diff -U 0 \"#{dest_path}\" \"#{source_path}\""

      return "- They're the same!" if txt.blank?

      txt = self.format txt, options


      # They're the same, so just blink message...

      if txt == ""
        return "- identical!" if ! display_in_view
        return View.flash "- identical!"
      end

      return txt if ! display_in_view

      # Display in a view

      View.to_buffer("do+compare+with")
      View.clear
      Notes.mode
      View.insert txt
      View.to_top

    end


    # Mapped to Ctrl+G
    def self.quit

      prefix = Keys.prefix

      # up+, so just suspend...
      if prefix == :u
        $el.suspend_emacs
        return
      end

      file = View.file
      existed_already = file && File.exists?(file)

      txt = Xiki['unsaved/', :go=>1]

      modified_files = txt.scan(/^= /)

      # Current file is .xiki and the only modified file, so auto-save it

      current_file_is_modified = file && View.modified?
      if modified_files.length == 1 && current_file_is_modified && View.extension == "xiki"
        options = {}
        options[:open_browser] = 1 if ! existed_already
        self.save options
        txt = ""
      end

      # No files to save, or all are "no changes" or "File no longer exists?"

      if self.nothing_unsaved txt

        # Unsaved "xsh" buffer, so save it...

        self.save_xsh_session   # Last file we were in (if buffer)

        # Save file and line number
        self.save_go_location


        # If xsh is open but not the one looking at, make a session for it
        if View.name != "xsh" && View.buffer_open?("xsh")
          $el.set_buffer "xsh"
          # Why calling it a 2nd time?
          self.save_xsh_session
        end


        # Save any shell commands we've run back to the external shell's history...

        txt = Shell.session_cache
        if txt
          txt.gsub!(/^\$ /, '')
          File.open(File.expand_path("~/.xiki/misc/tmp/recent_history_internal.xiki"), "w") { |f| f << txt }
        end

        $el.kill_emacs
      else
        View.open :txt=>"unsaved/\n#{txt.gsub /^/, '  '}", :line_found=>2, :name=>"unsaved/"
      end

      nil

    end

    def self.quit_and_run commands, options={}

      if options[:dir]
        dir = Shell.quote_file_maybe Bookmarks[options[:dir]]
        commands = "cd #{dir}\n#{commands}"
      end   #> !!!

      Xsh.save_go_commands commands
      self.quit
    end

    def self.nothing_unsaved txt
      txt == "- No files unsaved!\n" || txt !~ /^  (?!: no changes|: File no longer exists\?$)/
    end


    def self.save_xsh_session options={}

      # Delete "G" bookmark, so we'll know to go to the session upon ^G
      FileUtils.rm(File.expand_path "~/.xiki/bookmarks/g.xiki") rescue nil

      # Avoid saving certain buffers

      # Return if it has a file - it means it's not the "xsh" session (it's just a file with that name)
      return if View.file

      # Save session to note in sessions.xiki...

      heading = View.extract_suggested_filename_from_txt
      return if ! heading
      heading.gsub!("_", " ")

      # Add "@" to headings > so it'll be shared
      heading.gsub! /^/, "@ " if options[:shared]


      sessions_file = File.expand_path "~/xiki/sessions.xiki"
      txt = File.read(sessions_file) rescue ""
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      # If we reopen the session (^G), delete the old version
      if $el.boundp :session_heading
        txt = Notes.split(txt)
        txt.delete_if{|o| o=~ /\A#{Regexp.escape($el.elvar.session_heading)}$/}
        txt = txt.join
      end

      # Make names unique
      headings = txt.scan(/^> (.+)/).flatten
      while(headings.member? heading)
        heading.sub!(/(\d*)$/) { $1.to_i + 1 }
        heading.sub! /1$/, '2'
      end

      section = "> #{heading}\n#{View.txt.strip}\n\n\n"
      txt = "#{section}#{txt}"

      File.open(sessions_file, "w") { |f| f << txt }

      # Save dir and cursor position
      cursor = View.cursor
      File.open(File.expand_path("~/.xiki/misc/last_session_meta.xiki"), "w") { |f| f << "#{cursor}\n#{View.dir}" }

      $el.elvar.session_heading = "> #{heading}"

      heading

    end

    def self.save_go_location

      file = View.file

      # .save_xsh_session didn't deem to save this view, so we should ignore it too
      return if ! file

      dir = File.expand_path "~/.xiki/bookmarks"
      FileUtils.mkdir_p dir   # Make sure dir exists
      # Save this file in > "g" bookmark!
      File.open("#{dir}/g.xiki", "w") { |f| f << "#{file}\n" }

      # Store the shell current dir, which will be restored whin a ^G from bash is done
      tmp_dir = File.expand_path "~/.xiki/misc/tmp"
      FileUtils.mkdir_p tmp_dir   # Make sure dir exists
      File.open("#{tmp_dir}/go_location_cd_dir.xiki", "w") { |f| f << "#{Shell.dir}\n" }

      View.message "The next time you type ^G in bash, it'll go here."

    end

    def self.file_list options={}

      txt = File.read(File.expand_path("~/.xiki/misc/logs/difflog.xiki"), *Xiki::Files.encoding_binary) rescue nil

      return [] if ! txt

      # Avoids "invalid byte sequence in UTF-8" error
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      files = []

      # :tree_format, so return navigable tree with file indented...

      if options[:tree_format]
        txt.scan(/^.*\n  - .*$/){|m| files << "= #{m}"}
        return files.reverse.uniq
      end

      # No options, so return as a list of just file paths

      txt.scan(/^(.*)\n  - (.*)$/){|m| files << "#{m[0]}#{m[1]}"}
      return files.reverse.uniq

    end


    # Show diffs for just one file ("." for current)
    def self.show_edits_for_bookmark
      bm = Keys.input :timed=>true, :prompt=>"Enter a bookmark to search edits: "
      return Launcher.open("diff log/diffs/") if bm == "8" || bm == " "
      path = bm == "." ? View.file : "%#{bm}/"
      return Launcher.open("#{path}\n  = edits/")
    end

    def self.show_edits
      return Launcher.open("#{View.file}\n  = edits/")
    end

  end
end
