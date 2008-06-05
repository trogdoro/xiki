class Files
  extend ElMixin
  # Lets user open a file
  def self.open
    if elvar.current_prefix_arg
      find_file Keys.input(:prompt => "Open file: ")
    else
      find_file read_file_name("Open: ")
    end
  end
  #
  def self.save
    # If prefix, save as
    if elvar.current_prefix_arg
      write_file read_file_name("Save as: ")
    # Otherwise, difflog save
    else
      difflog_save
    end
    # Refresh
    self.refresh_me_and_next_dired
  end

  def self.copy
    #from = Files.file_name(:path => true)
    from = Files.file_name

    View.next
    to = View.dir
    View.previous

    if Keys.prefix_u
      rename_file(from, to)
    elsif Keys.prefix_uu
      command = "cp -R \"#{from}\" \"#{to}\""
      Shell.run command, :sync => true
    else
      copy_file(from, to)
    end

    # Refresh
    self.refresh_me_and_next_dired
  end

  def self.zip
    #from = Files.file_name(:path => true)
    from = Files.file_name
    other_window 1;  to = elvar.default_directory;  other_window -1
    if Keys.prefix_u
      command = "unzip -d \"#{to}\" \"#{from}\""
    else
      command = "zip -r \"#{to}#{from}.zip\" \"#{from}\""
    end
    message command
    shell_command command
  end

  def self.file_name options={}
    if options[:path]
      return buffer_file_name || dired_get_filename #'no_dir
    end
    if buffer_file_name
      return file_name_nondirectory(buffer_file_name)
    end
    dired_get_filename(:no_dir)
  end

  def self.refresh_me_and_next_dired
    revert_buffer(true, true, true) if(elvar.dired_directory)
    other_window 1
    revert_buffer(true, true, true) if(elvar.dired_directory)
    other_window -1
  end

  def self.open_tail
    Shell.run "tail -f #{View.file}", :buffer => "*tail of #{View.file}"
  end

  def self.menu
    puts "
      - .current 20/
      - .edited 20/
      - .history 20/
      "
  end

  def self.current times=nil
    times ||= History.prefix_times

    paths = ( buffer_list.map { |b| buffer_file_name(b) }.select{|path| path})
    paths = paths[0..(times-1)]
    puts CodeTree.tree_search + TreeLs.paths_to_tree(paths)
  end

  def self.edited times=nil
    times ||= History.prefix_times
    puts CodeTree.tree_search + TreeLs.paths_to_tree(elvar.editedhistory_history.to_a[0..(times-1)])
  end

  def self.history times=nil
    times ||= History.prefix_times
    puts CodeTree.tree_search + TreeLs.paths_to_tree(elvar.recentf_list.to_a[0..(times-1)])
  end

end

$el.el4r_lisp_eval("(require 'recentf)")
$el.el4r_lisp_eval("(recentf-mode 1)")
