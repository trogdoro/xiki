module Xiki
  class History

    #   FILENAME = File.expand_path "~/.emacs.d/menu_log.notes"
    # Unused
    def self.menu
      # Is this even called anymore?
      "
      > Files
      << =edited/
      << =recent files/
      << =unsaved/

      > See
      << =log/
      << =last/
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
      elsif path = options[:file]
        path = File.expand_path(path)

        paths = [path]
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

        View.to_buffer("outline/")
        View.clear;  Notes.mode

        raise "Thought this wouldn't happen :(" if paths.length > 1

        dir, file = paths[0].match(/(.+\/)(.+)/)[1..2]
        View << "#{dir}\n  - #{file}\n"

        View.to_top
        Keys.clear_prefix
        FileTree.select_next_file
        if options[:all]
          FileTree.enter_lines(//)
        elsif options[:outline] || options[:prompt_for_bookmark]
          FileTree.enter_lines
        else
          Tree.filter :recursive => true
        end
      end

    end

    def self.open_history
      times = self.prefix_times
      View.to_buffer("history/")
      View.clear;  notes_mode

      self.insert_history times
      View.to_top
      Keys.clear_prefix
      FileTree.select_next_file
      Tree.filter :recursive => true
    end

    def self.insert_history times
      View.insert Tree.paths_to_tree($el.elvar.recentf_list.to_a[0..(times-1)])
    end

    def self.enter_history
      orig = Location.new
      self.insert_history self.prefix_times
      right = $el.point
      orig.go
      Tree.filter :recursive => true, :left => $el.point, :right => right
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
      Tree.filter :recursive => true, :left => $el.point, :right => right
    end

    def self.backup_file

      dir = File.expand_path "#{Bookmarks[":x"]}misc/versions"
      FileUtils.mkdir_p dir   # Guarantee dir exists

      prefix = Keys.prefix

      # :-, so always use current file, not file path cursor is on...

      path = prefix == :- ?
        View.file :
        FileTree.tree_path_or_this_file

      name = path.sub(/.+\//, '')
      name = "#{dir}/#{name}.#{Time.now.strftime('%Y-%m-%d.%H-%M')}"
      if prefix == :u
        txt = Keys.input :prompt=>"Comment for this version: "
        name = "#{name}.#{TextUtil.snake txt}"
      end

      # Copy file
      $el.copy_file path, name

      message = "backed up to: #{Bookmarks.bookmarkify_path name}"
      View.flash "- #{message}"#, :times=>3
      View.message "Successfully #{message}"
    end

    def self.diff_with_backup
      # If up+, do interactive ediff...

      if Keys.prefix_u
        $el.ediff_files Dir["#{Bookmarks[':x/misc/versions']}#{View.file_name}*"].last, View.file
        return
      end

      backup = Dir["#{Bookmarks[':x/misc/versions']}/#{View.file_name}.????-??-??.??-??*"].last   # "

      return View.beep("- No backup exists in :x/misc/versions/") if ! backup

      file = View.file

      # If 8+, replace current version with backup...

      if Keys.prefix == 8
        return if ! View.confirm "Replace with contents of backup?  Afterwards you can save to make it permanent."
        orig = Location.new
        $el.insert_file_contents backup, nil, nil, nil, true
        orig.go
        return
      end

      # Just jump to it
      return View.open backup if Keys.prefix == 0

      # Reverse the files
      file, backup = backup, file if Keys.prefix == :-

      diff = Shell.run "diff -w -U 0 \"#{backup}\" \"#{file}\"", :sync=>true

      return Launcher.show "- No Differences!" if diff.blank?

      diff = DiffLog.format diff, :use_other_path=>1

      View.to_buffer("*diff with version*")
      View.clear
      Notes.mode

      $el.insert diff.count("\n") > 2 ?
        diff :
        "| Alert\n- ~No Differences~\n"

      View.to_top

    end

    def self.list
      regex = Regexp.quote View.name
      current_file = View.file.sub(/.+\//, "\\0\n  + ")
      Launcher.open "#{current_file}\n:x/misc/versions/\n  - ***^#{regex}\./"

      "to-does"
    end

    def self.init_in_client

      $el.el4r_lisp_eval %`
        (ol "init!")
        (progn
          (defun xiki-history-find-file-handler ()
            (ol "----xiki-history-find-file-handler")
            (el4r-ruby-eval "Xiki::History.log")
          )
          (add-hook 'find-file-hook 'xiki-history-find-file-handler)
        )
      `

    end

    def self.log

      tmp_dir = File.expand_path "~/xiki/misc/logs"
      FileUtils.mkdir_p tmp_dir   # Make sure dir exists
      file = "#{tmp_dir}/recent.notes"
      File.open(file, "a") { |f| f << "#{View.file}\n" }

    end

  end

end
