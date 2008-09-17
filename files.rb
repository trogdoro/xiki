class Files
  extend ElMixin

  @@dir_hash = {
    'j' => '.js',
    'r' => '.rb',
    'r' => '/spec/',
    'm' => '/app/models/',
    'v' => '/app/views/',
    'c' => '/app/controllers/',
    'n' => '.notes',
    }

  # Lets user open a file
  def self.open
    if elvar.current_prefix_arg
      find_file read_file_name("Open: ")
    else
      find_file Keys.input(:prompt => "Open file: ")
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

    if Keys.prefix_u?
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
    if Keys.prefix_u?
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
      + .current 20/
      + .edited 20/
      + .history 20/
      "
  end

  def self.edited_array
    elvar.editedhistory_history.to_a
  end

  def self.edited times=nil, options={}
    times ||= History.prefix_times
    paths = edited_array[0..(times-1)]
    if options[:dir]
      paths = paths.grep(Regexp.new(Regexp.escape(options[:dir])))
    end
    puts CodeTree.tree_search + TreeLs.paths_to_tree(paths)
  end

  def self.history_array
    elvar.recentf_list.to_a
  end

  def self.history times=nil
    times ||= History.prefix_times
    puts CodeTree.tree_search + TreeLs.paths_to_tree(history_array[0..(times-1)])
  end

  def self.open_just
    key = Keys.input(:prompt => "Open just currently open files of type (enter a letter): ", :one_char => true)
    dir = @@dir_hash[key]
    return message("No dir matching '#{key}' found.  See Files\#@@dir_hash") unless dir
    if Keys.prefix_u?
      Keys.clear_prefix
      CodeTree.display_menu("Files.edited(100, :dir => '#{dir}')")
    else
      CodeTree.display_menu("Buffers.tree(0, :dir => '#{dir}')")
    end
  end

  def self.open_last
    key = Keys.input(:prompt => "Open just currently open files of type (enter a letter): ", :one_char => true)
    dir = @@dir_hash[key]
    View.open Files.edited_array.grep((Regexp.new(Regexp.escape(dir)))).first
  end

  def self.open_edited
    case Keys.prefix
    when nil:  CodeTree.display_menu("Files.edited 20")
    when 0:  CodeTree.display_menu("Files.edited")
    else  CodeTree.display_menu("Files.edited #{Keys.prefix}")
    end
  end

  def self.open_history
    case Keys.prefix
    when nil:  CodeTree.display_menu("Files.history 20")
    when 0:  CodeTree.display_menu("Files.history")
    else  CodeTree.display_menu("Files.history #{Keys.prefix}")
    end
  end


  def self.open? name
    buffer_list.find{|b| buffer_file_name(b) == name}
  end

  def self.open_in_os path=nil
    path ||= View.file
    path ||= View.dir
    shell_command("open #{path}")
  end

end

$el.el4r_lisp_eval("(require 'recentf)")
$el.el4r_lisp_eval("(recentf-mode 1)")
