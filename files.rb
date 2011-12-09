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
    "
    - @current/
    - @edited/
    - .history/
    - docs/
      | Show files currently open
      - @current/
      |
      | Show files recently edited
      - @edited/
      |
      | Show files recently open
      - @files/history/
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

  def self.history # *path
    paths = history_array#[0..400]
    paths.map!{|i| i.sub(/(.+\/)(.+)/, "- @\\1\n  - \\2")}
    paths.join("\n")
  end

  def self.history_array
    elvar.recentf_list.to_a
  end

  def self.history_tree times=nil
    times ||= History.prefix_times
    puts CodeTree.tree_search_option + FileTree.paths_to_tree(history_array[0..(times-1)])
  end

  def self.open_just
    key = Keys.input(:prompt => "Open just currently open files of type (enter a letter): ", :chars=>1)
    dir = @@dir_hash[key]
    return message("No dir matching '#{key}' found.  See Files\#@@dir_hash") unless dir
    if Keys.prefix_u?
      Keys.clear_prefix
      Launcher.open("- Files.edited(100, :dir => '#{dir}')/")
    else
      Launcher.open("- Buffers.tree(0, :dir => '#{dir}')/")
    end
  end

  def self.open_last
    key = Keys.input(:prompt => "Open just currently open files of type (enter a letter): ", :chars=>1)
    dir = @@dir_hash[key]
    View.open Files.edited_array.grep((Regexp.new(Regexp.escape(dir)))).first
  end

  def self.open_edited
    Launcher.open("- edited/")
    #     case Keys.prefix
    #     when nil:  Keys.prefix = nil; Launcher.open("- Files.edited_flat/")
    #     when 0:  Launcher.open("- Files.edited/")
    #     when :u:  Launcher.open("- Files.edited 7/")
    #     else  Launcher.open("- Files.edited #{Keys.prefix}/")
    #     end
  end

  def self.open_history
    case Keys.prefix
    when nil:  Keys.prefix = nil; Launcher.open("- Files.history/")
    when 0:  Launcher.open("- Files.history_tree/")
    when :u:  Launcher.open("- Files.history_tree 7/")
    else  Launcher.open("- Files.history_tree #{Keys.prefix}/")
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
    path = File.expand_path(Keys.bookmark_as_path(:include_file=>1))
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

  # Returns contents of a dir.  Just a wrapper around Dir.entries that removes "." and ".."
  def self.in_dir path
    Dir.entries(path).select{|o| o !~ /^\.+$/}
  end

end

$el.el4r_lisp_eval("(require 'recentf)")
$el.el4r_lisp_eval("(recentf-mode 1)")
