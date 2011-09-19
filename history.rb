class History
  extend ElMixin

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
      bm = Keys.input(:timed => true, :prompt => "Enter bookmark to show outline for: ")
      path = Bookmarks.expand(bm, :just_bookmark => true)
      path = File.expand_path(path)
      paths = [path]
    elsif options[:outline] || options[:all]

      paths = [buffer_file_name(buffer_list[0])]
    else  # No options passed
      times = Keys.prefix
      paths = ( buffer_list.map { |b| buffer_file_name(b) }.select{|path| path})
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
      View.clear;  notes_mode
      View.insert FileTree.paths_to_tree(paths)
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
    times = self.prefix_times
    View.to_buffer("*tree of edited")
    View.clear;  notes_mode
    View.insert FileTree.paths_to_tree(elvar.editedhistory_history.to_a[0..(times-1)])
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
    insert FileTree.paths_to_tree(elvar.recentf_list.to_a[0..(times-1)])
  end

  def self.enter_history
    orig = Location.new
    self.insert_history self.prefix_times
    right = point
    orig.go
    Tree.search :recursive => true, :left => point, :right => right
  end

  def self.insert_viewing times
    paths = ( buffer_list.map { |b| buffer_file_name(b) }.select{|path| path})
    paths = paths[0..(times-1)] if times  # Limit to number if prefix passed
    insert FileTree.paths_to_tree(paths)
  end

  def self.enter_viewing
    orig = Location.new
    self.insert_viewing self.prefix_times
    right = point
    orig.go
    Tree.search :recursive => true, :left => point, :right => right
  end

  def self.open_unsaved

    # Get unsaved paths
    buffer_modified_p

    # Narrow down to modified buffer only
    modified = buffer_list.to_a.
      select{|b| buffer_modified_p(b)}.
      select{|b| buffer_file_name(b)}.
      map{|b| buffer_file_name(b)}

    View.to_buffer("*tree of unsaved buffers")
    View.clear;  notes_mode
    if (modified.size == 0)
      return insert("| Note\n- ~No Buffers Unsaved~\n")
    end
    View.insert FileTree.paths_to_tree(modified)
    View.to_top
    Keys.clear_prefix
    FileTree.select_next_file
    Tree.search :recursive => true
  end

  def self.setup_editedhistory
    el4r_lisp_eval %q<
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
    unless bm.any?   # If no bookmark, just show error
      View.beep
      return View.message("Error: create a bookmark named 'bak' first, in a dir where you backups will go.")
    end

    path = Keys.prefix_u? && FileTree.handles? ?   # If backup file in tree
      Tree.construct_path :
      View.file

    name = path.sub(/.+\//, '')

    # Copy file
    $el.copy_file path, "#{bm}#{name} #{Time.now.strftime('%Y-%m-%d %H-%M')}"

    View.message "Successfully backed up '#{name}' to $bak/"
  end

  def self.diff_with_backup

    if Keys.prefix_u
      $el.ediff_files Dir["#{Bookmarks['$bak']}#{View.file_name}*"].last, View.file
      return
    end

    backup = Dir["#{Bookmarks['$bak']}#{View.file_name}*"].last
    diff = Console.run "diff -w -U 0 \"#{backup}\" \"#{buffer_file_name}\"", :sync=>true
    diff = DiffLog.format(View.path, View.file_name, diff)

    View.to_buffer("*diff with saved*")
    View.clear
    notes_mode

    insert diff.count("\n") > 2 ?
      diff :
      "| Alert\n- ~No Differences~\n"

  end

end
History.setup_editedhistory
