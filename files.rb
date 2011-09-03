# -*- coding: utf-8 -*-
class Files
  extend ElMixin

  @@dir_hash = {
    'j' => '.js',
    'r' => '.rb',
    's' => '/spec/',
    'a' => '/app/',
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

  def self.open_sudo
    find_file "/sudo:root@localhost:#{View.file || View.dir}"
  end

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
      Console.run command, :sync => true
    else
      $el.copy_file(from, to)
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
    $el.shell_command command
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
    bm = Keys.input(:timed=>true, :prompt=>"Enter bookmark of file to tail (or period for current file): ")
    file = (bm == ".") ?
      View.file :
      Bookmarks["$#{bm}"]
    Console.run "tail -f #{file}", :buffer => "*tail of #{file}"
  end

  def self.menu
    puts "
      + .edited 20/
      + .history 20/
      + .visiting 20/
      "
  end

  def self.edited_array
    elvar.editedhistory_history.to_a
  end

  def self.current times=nil
    Buffers.tree times
  end

  def self.edited times=nil, options={}
    times ||= History.prefix_times
    paths = edited_array[0..(times-1)]
    if options[:dir]
      paths = paths.grep(Regexp.new(Regexp.escape(options[:dir])))
    end
    puts CodeTree.tree_search_option + FileTree.paths_to_tree(paths)
  end

  def self.edited_flat
    paths = edited_array[0..120]
    paths.map!{|i| i.sub(/(.+\/)(.+)/, "- \\1\n  - \\2")}
    CodeTree.tree_search_option + paths.join("\n")
  end

  def self.history_flat
    paths = history_array[0..400]
    paths.map!{|i| i.sub(/(.+\/)(.+)/, "- \\1\n  - \\2")}
    CodeTree.tree_search_option + paths.join("\n")
  end

  def self.history_array
    elvar.recentf_list.to_a
  end

  def self.history times=nil
    times ||= History.prefix_times
    puts CodeTree.tree_search_option + FileTree.paths_to_tree(history_array[0..(times-1)])
  end

  def self.open_just
    key = Keys.input(:prompt => "Open just currently open files of type (enter a letter): ", :one_char => true)
    dir = @@dir_hash[key]
    return message("No dir matching '#{key}' found.  See Files\#@@dir_hash") unless dir
    if Keys.prefix_u?
      Keys.clear_prefix
      CodeTree.display_menu("- Files.edited(100, :dir => '#{dir}')/")
    else
      CodeTree.display_menu("- Buffers.tree(0, :dir => '#{dir}')/")
    end
  end

  def self.open_last
    key = Keys.input(:prompt => "Open just currently open files of type (enter a letter): ", :one_char => true)
    dir = @@dir_hash[key]
    View.open Files.edited_array.grep((Regexp.new(Regexp.escape(dir)))).first
  end

  def self.open_edited
    case Keys.prefix
    when nil:  Keys.prefix = nil; CodeTree.display_menu("- Files.edited_flat/")
    when 0:  CodeTree.display_menu("- Files.edited/")
    when :u:  CodeTree.display_menu("- Files.edited 7/")
    else  CodeTree.display_menu("- Files.edited #{Keys.prefix}/")
    end
  end

  def self.open_history
    case Keys.prefix
    when nil:  Keys.prefix = nil; CodeTree.display_menu("- Files.history_flat/")
    when 0:  CodeTree.display_menu("- Files.history/")
    when :u:  CodeTree.display_menu("- Files.history 7/")
    else  CodeTree.display_menu("- Files.history #{Keys.prefix}/")
    end
  end

  def self.open? name
    buffer_list.find{|b| buffer_file_name(b) == name}
  end

  def self.open_in_os path=nil
    path ||= View.file
    path ||= View.dir
    $el.shell_command("open \"#{path}\"")
  end

  def self.do_load_file
    $el.revert_buffer(true, true, true)

    View.message "Reverted file"

    return if ! Keys.prefix_u

    View.message $el.auto_revert_mode ? "Enabled Auto-revert" : "Disabled Auto-revert"
  end

  def self.do_clean_quotes
    with(:save_excursion) do
      $el.clean_crazy_quotes
    end
  end

  # Use File.basename
  #   def self.name path   # Extract name from path
  #     path.sub(/.+\/(.+)/, "\\1")   # Cut of path
  #   end

  # Use File.dirname
  #   def self.dir path   # Extract dir from path
  #     return "" unless path =~ /\//
  #     path.sub(/(.+)\/.*/, "\\1")   # Cut of path
  #   end

  def self.enter_file
    path = File.expand_path(Keys.bookmark_as_path(:include_file=>true))
    path << "/" if File.directory? path
    View.insert(path)
  end

  # This is currently mac-specific
  def self.open_last_screenshot

    dirs = `ls -t #{Bookmarks["$dt"]}`
    screenshot = Bookmarks["$dt"]+dirs[/.+/]

    self.open_as screenshot, "Adobe Illustrator"

  end

  # This is currently mac-specific
  def self.open_as file, application
    command = "open -a \"#{application}\" \"#{file}\""

    $el.shell_command(command)
  end

  def self.open_in_window

    file = FileTree.tree_path_or_this_file

    # Else, reveal current file
    command = "open --reveal \"#{file}\""
    $el.shell_command(command)

  end

  def self.dir_of path
    if File.directory? path
      path =~ /\/$/ ? path : "#{path}/"
    else
      path[/.+\//]
    end
  end

  def self.append path, txt

    return if View.name =~ /_log.notes$/
    path = File.expand_path path
    txt = "#{txt.strip}\n"
    File.open(path, "a") { |f| f << txt }
  end

end

$el.el4r_lisp_eval("(require 'recentf)")
$el.el4r_lisp_eval("(recentf-mode 1)")
