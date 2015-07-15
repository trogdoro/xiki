require 'xiki/core/hide'

module Xiki
  # Will store a diff each time a file is saved.
  class DiffLog

    @@log = File.expand_path("~/xiki/misc/logs/difflog.notes")
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
        path = Files.tilda_for_home path
        regex = /^#{Regexp.escape File.dirname path}\/\n  - #{Regexp.escape File.basename path}/
      else   # Dir
        path = Files.tilda_for_home path
        regex = /^#{Regexp.escape path}/
        # Was doing nothing
        #         path = "#{path}/" if path !~ /\/$/
      end
      txt = txt.select{|o| o =~ regex}
      "=#{txt.join("=")}"
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
    def self.save_newly_created

      # Store original buffer in local var, so we can make sure we go back to the right place
      buffer_to_save = View.name

      name = View.extract_suggested_filename_from_txt
      name = "#{name}.notes"

      View.to_buffer "save as/"
      Notes.mode
      View.delete_all
      $el.make_local_variable :buffer_to_save
      $el.elvar.buffer_to_save = buffer_to_save

      View >> "\n\n\n"
      View << "save as/"
      Tree.<< X("save as/", :save_as_name=>name), :no_search=>1
      Search.forward "\\.", :beginning=>1

    end

    def self.save options={}

      if View.name == "diff with saved/"   # If viewing diff, close it and save the actual file...
        file = View.txt[/.+\n.+/].sub("\n  - ", "")
        View.kill
        View.open file
      end
      prefix = Keys.prefix :clear=>1

      # It's unsaved and not associated with a file yet, so prompt for where to save it
      return self.save_newly_created if ! View.file

      self.save_diffs if prefix != :u && ! options[:no_diffs]

      $el.save_buffer

      if prefix == :-   # Dash+, so load in browser after
        sleep(0.3)
        Firefox.reload
      elsif prefix == 9
        Launcher.do_last_launch
      end
      View.message ""
    end

    def self.format raw, options={}
      if options[:use_other_path]
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

      # =commit/Quotes > swapped quotes for colons.

      # Make - and + lines into -| and +| lines
      raw.gsub!(/^\+(.*)/, "      :+\\1")
      raw.gsub!(/^-(.*)/, "      :-\\1")

      path = Files.tilda_for_home path

      # Return with path
      "#{path}\n" +
      "  - #{file}\n" +
      raw
    end

    def self.compare_with_saved

      # up+up+, so do side-by-side...

      if Keys.prefix_u
        return Launcher.open("unsaved/", :name=>"unsaved changes")
      end

      if Keys.prefix_uu

        buffer_unsaved = View.buffer

        path = View.file

        View.to_buffer "untitled"
        $el.rename_uniquely
        View << File.read(path)

        return $el.ediff_buffers View.buffer, buffer_unsaved

      end

      diff = self.save_diffs :just_return=>1
      View.message ""   # To avoid message when writing
      diff = "" if diff.nil?
      no_difference = diff.count("\n") <= 2

      if no_difference
        $el.set_buffer_modified_p nil
      end

      View.to_buffer("diff with saved/")
      View.clear
      Notes.mode

      # No differences, so just say so...

      if no_difference #diff.count("\n") <= 2
        View.insert("- No unsaved changes!\n\nclose view/\n")
        Line.previous 1
      else
        View.insert diff + "\nsave\nsave without diff\nrevert\n"
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
        File.open(@@temp_path, "w") { |f| f << options[:texta] }
      else
        patha = View.file
        $el.write_region nil, nil, @@temp_path
      end

      # New file, so show all as new...

      diff =
        if patha
          txt = Shell.sync "diff -U 0 \"#{patha}\" \"#{@@temp_path}\""

          return if txt.empty? || txt =~ /: No such file or directory\n/   # Fail gracefully if file didn't exist
          self.format(txt) rescue nil
        else
          txt = View.txt
          txt.gsub! /^/, "      :+"
          "#{View.dir}/\n  - #{View.name}\n    :1\n#{txt}\n\n| Todo > =save doesn't work yet with new files"
        end

      return diff if ! diff || options[:just_return]

      FileUtils.mkdir_p File.dirname(@@log)

      File.open(@@log, "a") { |f| f << diff } unless diff.count("\n") <= 2
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
      txt = self.format txt

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
    def self.grab options={}

      # Just quit if any prefix
      return $el.keyboard_quit if Keys.prefix

      # Grab line and write to file...

      path = Tree.path
      last = Path.split path[-1]

      view_dir = View.dir

      # If just $... (Is this too liberal?)

      if FileTree.handles?(path[-1])

        # /foo/, so just cd to the dir

        # Could be /tmp, "/tmp/$ pwd", or "/tmp/$ pwd/item"

        dir = Bookmarks[path[-1]]
        command = nil

        dir.sub!(/\/$/, '')

        # Add quotes unless it's all slashes and letters
        dir = "\"#{dir}\"" if dir !~ /^[a-z_\/.-]+$/i

        commands = "cd #{dir}\n"
        commands << command if command

        return self.quit_and_run commands
      end

      # If ls, grab dir and exit > hard-coded support for ls, for now > abstract out for other commands later

      if last[0] == "$ ls"

        dir = last[1..-1].map{|o| o.sub(/^: /, '')}.join()

        if dir =~ /\/$/
          commands = "cd #{Shell.quote_file_maybe dir}"
        else
          editor = ENV['EDITOR']

          # Non-console editor, so don't quit
          if ["subl"].member? editor
            command = "#{ENV['EDITOR']} #{view_dir}/#{Shell.quote_file_maybe dir}"
            Shell.command command
            return
          end

          commands = "#{ENV['EDITOR']} #{Shell.quote_file_maybe dir}"
        end

        return self.quit_and_run commands
      end

      # Not $..., so explain why we can't expand...

      if last[-1] !~ /^[$%] (.+)/

        # Just exit
        return View.flash("- Can only grab commands and dirs!")
      end

      command = $1

      ancestors = options[:ancestors] || Tree.ancestors

      # /foo/$..., so grab command and dir...

      if ancestors && FileTree.handles?(ancestors[-1])

        dir = ancestors[-1]

        dir = Bookmarks[dir]
        dir.gsub! "//", "/"   # Under a file command, so change "/foo//" to "/foo/" before cd'ing

        dir = Shell.quote_file_maybe dir

        # Todo > Do this, when there are args?
        #           # Pop off any others at end that are $... or ~...
        #           while last[-1] =~ /^[~$%&]/
        #             last.pop
        #           end

        commands = "cd #{dir}\n#{command}"
        self.quit_and_run commands

      end

      # $..., so grab command, and figure out the dir...

      # They cd'ed in this session > so do cd in the shell...

      session_dir = File.expand_path Bookmarks.bookmarks_optional("se")
      in_session_dir = view_dir == session_dir


      # If session, do based on view dir
      orig_dir = Xsh.determine_session_orig_dir(View.file_name) if in_session_dir

      # If couldn't find, or normal dir, default to current dir

      orig_dir ||= view_dir
      orig_dir.sub! /\/$/, ''   # To ensure a fair comparison

      commands = ""

      # They did a cd, so add it...

      view_file = View.file

      dir = nil
      dir = Shell.dir
      dir.sub!(/\/$/, '') if dir != "/"

      # If it's a view without a file, always do cd
      if ! View.file || orig_dir != dir   # Or if they changed the dir
        dir = "\"#{dir}\"" if dir !~ /^[a-z_\/.-]+$/i
        commands << "cd #{dir}\n"
      end

      commands << "#{command}\n"

      self.quit_and_run commands

    end

    def self.quit

      prefix = Keys.prefix

      # up+, so just suspend...
      if prefix == :u
        $el.suspend_emacs
        return
      end

      txt = X'unsaved'


      # No files to save, or all are "no changes" or "File no longer exists?"


      # if txt == "- No files unsaved!\n" || txt !~ /^  (?!: no changes|: File no longer exists\?$)/
      if self.nothing_unsaved txt

        # Unsaved "xsh" buffer, so save it...

        self.save_xsh_sessions   # Last file we were in (if buffer)

        self.save_grab_location   # Save file and line number

        # If not already there, but xsh is open
        if View.name != "xsh" && View.buffer_open?("xsh")
          $el.set_buffer "xsh"
          self.save_xsh_sessions
        end

        # Save any shell commands we've run back to the external shell's history...

        txt = Shell.session_cache
        if txt
          txt.gsub!(/^\$ /, '')
          File.open(Bookmarks[":x/misc/tmp/recent_history_internal.notes"], "w") { |f| f << txt }
        end

        $el.kill_emacs
      else
        View.open :txt=>"unsaved/\n#{txt.gsub /^/, '  '}", :line_found=>2, :name=>"unsaved/"
      end

      nil

    end

    def self.quit_and_run commands, options={}

      if options[:dir]
        dir = Shell.quote_file_maybe options[:dir]
        commands = "cd #{dir}\n#{commands}"
      end

      Xsh.save_grab_commands commands
      self.quit
    end

    def self.nothing_unsaved txt
      txt == "- No files unsaved!\n" || txt !~ /^  (?!: no changes|: File no longer exists\?$)/
    end


    def self.save_xsh_sessions

      # Avoid saving certain buffers
      View.kill if ["unsaved/"].member?(View.name)

      return if View.file   # Return if it has a file - it means it's not the "xsh" session (it's just a file with that name)

      # Save session to file...

      name = View.extract_suggested_filename_from_txt

      return if ! name

      sessions_dir = File.expand_path "~/xiki/sessions"

      FileUtils.mkdir_p sessions_dir   # Make sure dir exists

      file = File.expand_path "#{sessions_dir}/#{name}.notes"
      file = Files.unique_name file

      $el.write_file file

      # Save which dir it was in to corresponding file in misc/sessions_dirs...

      sessions_dirs_file = file.sub "sessions/", "misc/sessions_dirs/"
      FileUtils.mkdir_p File.expand_path("~/xiki/misc/sessions_dirs/")
      File.open(sessions_dirs_file, "w") { |f| f << Shell.dir }

      file

    end

    def self.save_grab_location #file

      file = View.file

      # .save_xsh_sessions didn't deem to save this view, so we should ignore it too
      return if ! file

      dir = File.expand_path "~/xiki/bookmarks"
      FileUtils.mkdir_p dir   # Make sure dir exists
      File.open("#{dir}/g.notes", "w") { |f| f << "#{file}\n" }

    end

    def self.file_list options={}

      txt = File.read File.expand_path("~/xiki/misc/logs/difflog.notes"), *Xiki::Files.encoding_binary
      # Avoids "invalid byte sequence in UTF-8" error
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      files = []

      # :tree_format, so return navigable tree with file indented...

      if options[:tree_format]
        txt.scan(/^.*\n  - .*$/){|m| files << "=#{m}"}
        return files.reverse.uniq
      end

      # No options, so return as a list of just file paths

      txt.scan(/^(.*)\n  - (.*)$/){|m| files << "#{m[0]}#{m[1]}"}
      return files.reverse.uniq

    end


    # Show diffs for just one file ("." for current)
    def self.show_edits

      bm = Keys.input :timed=>true, :prompt=>"Enter a bookmark to search edits: "
      return Launcher.open("diff log/diffs/") if bm == "8" || bm == " "
      path = bm == "." ? View.file : ":#{bm}/"
      return Launcher.open("#{path}\n  =edits/")
    end

  end
end
