module Xiki
  class History

    #   FILENAME = File.expand_path "~/.emacs.d/menu_log.notes"
    # Unused
    def self.menu
      # Is this even called anymore?
      "
      > Files
      << @edited/
      << @recent_files/
      << @not saved/

      > See
      << @log/
      << @last/
      "
    end

    def self.prefix_times
      prefix = Keys.prefix
      prefix ||= 20
      prefix = 20 if prefix == :u
      prefix
    end

    def self.open_current options={}
      if options[:paths]
        paths = options[:paths]
      elsif options[:prompt_for_bookmark]
        bm = Keys.input(:timed => true, :prompt => "Enter bookmark to show content for: ")
        path = Bookmarks.expand(bm, :just_bookmark => true)
        return View.beep("- Bookmark '#{bm}' not found!") if ! path
        path = File.expand_path(path)

        if ! options[:all] && View.files.member?(path)
          View.open path
          FileTree.to_outline
          return
        end

        paths = [path]
      elsif options[:outline] || options[:all]
        paths = [$el.buffer_file_name($el.buffer_list[0])]
      else  # No options passed
        times = Keys.prefix
        paths = ( $el.buffer_list.map { |b| $el.buffer_file_name(b) }.select{|path| path})
        paths = paths[0..(times-1)] if times  # Limit to number if prefix passed
      end

      if options[:enter_here]  # If entering in current file
        path = paths.to_s
        # If it's a dir, delegate to Open Tree
        if path =~ /\/$/
          FileTree.ls :here => true, :dir => path
          return
        end
        View.insert "- " + FileTree.filename_to_next_line(paths.to_s)
        $el.open_line 1
        FileTree.enter_lines

      else  # If entering in new buffer

        # By default happen in same view
        View.bar if options[:bar]  # If to go to bar

        # If only one path, go to it's buffer if already open
        if paths.size == 1
          View.open paths[0]
        end

        View.to_buffer("*tree of current")
        View.clear;  Notes.mode

        raise "Thought this wouldn't happen :(" if paths.length > 1

        dir, file = paths[0].match(/(.+\/)(.+)/)[1..2]
        View << "- #{dir}\n  - #{file}\n"

        View.to_top
        Keys.clear_prefix
        FileTree.select_next_file
        if options[:all]
          FileTree.enter_lines(//)
        elsif options[:outline] || options[:prompt_for_bookmark]
          FileTree.enter_lines
        else
          Tree.search :recursive => true
        end
      end

    end

    def self.open_edited
      # TODO: Is this used any more? > moved to @edited
      times = self.prefix_times
      View.to_buffer("*tree of edited")
      View.clear;  notes_mode
      View.insert Tree.paths_to_tree($el.elvar.editedhistory_history.to_a[0..(times-1)])
      View.to_top
      Keys.clear_prefix
      FileTree.select_next_file
      Tree.search :recursive => true
    end

    def self.open_history
      times = self.prefix_times
      View.to_buffer("*tree of history")
      View.clear;  notes_mode

      self.insert_history times
      View.to_top
      Keys.clear_prefix
      FileTree.select_next_file
      Tree.search :recursive => true
    end

    def self.insert_history times
      View.insert Tree.paths_to_tree($el.elvar.recentf_list.to_a[0..(times-1)])
    end

    def self.enter_history
      orig = Location.new
      self.insert_history self.prefix_times
      right = $el.point
      orig.go
      Tree.search :recursive => true, :left => $el.point, :right => right
    end

    def self.insert_viewing times
      paths = ( $el.buffer_list.map { |b| $el.buffer_file_name(b) }.select{|path| path})
      paths = paths[0..(times-1)] if times  # Limit to number if prefix passed
      View.insert Tree.paths_to_tree(paths)
    end

    def self.enter_viewing
      orig = Location.new
      self.insert_viewing self.prefix_times
      right = $el.point
      orig.go
      Tree.search :recursive => true, :left => $el.point, :right => right
    end

    def self.setup_editedhistory

      return if ! $el

      $el.el4r_lisp_eval %q<
        (progn
          ; Settings
          (setq editedhistory-log "~/.editedhistory")

          ; Runs upon startup.  Load log file into memory if it exists.  Run this manually if you edit the log file.
          (defun editedhistory-load ()
            (if (file-readable-p editedhistory-log)
              ; Read from file into memory
              (with-temp-buffer
                (insert-file-contents editedhistory-log nil nil nil t)
                (setq editedhistory-history
                  (car (read-from-string (buffer-substring (point-min) (point-max))) )))
              ; Otherwise, initialize var
              (set 'editedhistory-history nil)))

          ; Saves on exit: 'editedhistory-history' to the file 'editedhistory-log'
          (add-hook 'kill-emacs-hook 'editedhistory-save)
          (defun editedhistory-save () (interactive)
            (with-temp-buffer
              (erase-buffer)
              (insert (pp-to-string editedhistory-history) )
              (if (file-writable-p editedhistory-log)
                (write-region (point-min) (point-max) editedhistory-log)
              )
              (kill-buffer (current-buffer)))
            nil)

          ; Runs upon save: Track modified files
          (defun editedhistory-remember-file ()
            ; Remove from list in case it's already there (we want to add to beginning)
            (when (boundp 'editedhistory-history)
              (setq editedhistory-history (delete buffer-file-name editedhistory-history) )
              ; Add to list
              (add-to-list 'editedhistory-history buffer-file-name))
            nil)   ; Return nil so we won't block writing
          (add-hook 'write-file-hooks 'editedhistory-remember-file)

          ; Load upon startup, to add hooks
          (define-minor-mode editedhistory-mode
            "Toggle editedhistory mode"
            :global t
            :group 'editedhistory

            ; Load if not yet loaded
            (pp (boundp 'editedhistory-loaded-p))
            (unless (boundp 'editedhistory-loaded-p)
              (setq editedhistory-loaded-p t)
              (pp editedhistory-loaded-p)
              ; Load log file into memory
              (editedhistory-load)))

          ; Load mode
          (editedhistory-mode t)
        )
      >
    end

    def self.backup_file
      bm = Bookmarks['$bak']
      return View.beep "- First, set the 'bak' bookmark to a dir!" if bm == "$bak"

      unless bm.any?   # If no bookmark, just show error
        View.beep
        return View.message("Error: create a bookmark named 'bak' first, in a dir where you backups will go.")
      end

      path = Keys.prefix_u? ?
        View.file :
        FileTree.tree_path_or_this_file

      name = path.sub(/.+\//, '')

      # Copy file xx
      $el.copy_file path, "#{bm}#{name}.#{Time.now.strftime('%Y-%m-%d.%H-%M')}"

      message = "backed up to $bak: '#{name}'"
      View.flash "- #{message}", :times=>3
      View.message "Successfully #{message}"
    end

    def self.diff_with_backup
      if Keys.prefix_u
        $el.ediff_files Dir["#{Bookmarks['$bak']}#{View.file_name}*"].last, View.file
        return
      end

      backup = Dir["#{Bookmarks['$bak']}#{View.file_name}.????-??-??.??-??"].last   # "

      return View.beep("- No backup exists in $bak/") if ! backup

      file = View.file

      # Replace current version with last
      if Keys.prefix == 8
        return if ! View.confirm "Replace current file with backup?"
        txt = File.read backup
        line = View.line
        View.kill_all
        View << txt
        View.line = line
        return
      end

      # Just jump to it
      return View.open backup if Keys.prefix == 0

      # Reverse the files
      file, backup = backup, file if Keys.prefix == :-

      diff = Console.run "diff -U 0 \"#{backup}\" \"#{file}\"", :sync=>true

      return Launcher.show "- No Differences!" if diff.blank?

      diff = DiffLog.format diff, :use_other_path=>1

      View.to_buffer("*diff with saved*")
      View.clear
      Notes.mode

      $el.insert diff.count("\n") > 2 ?
        diff :
        "| Alert\n- ~No Differences~\n"

    end

  end
  History.setup_editedhistory
end
