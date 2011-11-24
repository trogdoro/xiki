require 'open3'

class Console
  extend ElMixin

  @@log = File.expand_path("~/.emacs.d/console_log.notes")

  def self.menu
    %`
    - commands you've run) .log
    - commands in buffers) .tree
    |
    > Run OS commands
    | In console (asynchronously)
    @Console.run "ls", :dir=>"/tmp"
    |
    | Inline (synchronously)
    @Console.run "ls", :dir=>"/etc", :sync=>true
    `
  end

  def self.log
    View.open @@log
  end

  # Run the command in a console
  def self.[] command
    self.run command, :sync=>1
  end

  def self.sync command
    self.run command, :sync=>1
  end

  def self.run command, options={}

    dir = options[:dir]
    sync = options[:sync]
    buffer = options[:buffer]
    reuse_buffer = options[:reuse_buffer]

    # Nil out dir if blank
    dir = nil if dir && dir.length == 0

    if dir
      dir = Bookmarks.expand(dir)
      # If relative dir, make current dir be on end of current
      dir = "#{elvar.default_directory}/#{dir}" unless dir =~ /^\//
      dir.gsub!(/\/\/+/, '/')

      # If file, but not dir, try backing up to the dir
      if File.exists?(dir) && ! File.directory?(dir)
        dir.sub!(/[^\/]+$/, '')
      end

      # If dir exists, continue
      if File.directory?(dir)
        # Put slash on end if not there
        dir = "#{dir}/" unless dir =~ /\/$/
      else  # Otherwise, exit
        return puts("#{dir} is not a dir")
      end
    else
      dir = elvar.default_directory
    end

    if sync

      return command if options[:no_enter]
      profile = File.exists?(File.expand_path('~/.profile')) ? '. ~/.profile;' : ''
      stdin, stdout, stderr = Open3.popen3("#{profile}cd \"#{dir}\";#{command}")
      result = ""
      result << stdout.readlines.join('')
      result << stderr.readlines.join('')
      result.gsub!("\c@", '.')   # Replace out characters that el4r can't handle
      return result

    else
      if View.in_bar? and ! options[:dont_leave_bar]
        View.to_after_bar
      end
      buffer ||= "*console #{dir}"

      if ! reuse_buffer
        buffer = generate_new_buffer(buffer)
      end
      View.to_buffer buffer
      erase_buffer if reuse_buffer
      elvar.default_directory = dir if dir
      $el.shell current_buffer
      Move.bottom
      if command  # If nil, just open console
        insert command
        Console.enter unless options[:no_enter]
      end
    end

    nil
  end

  def self.open dir=nil
    View.handle_bar
    dir ||= elvar.default_directory
    dir = File.expand_path(dir)+"/"
    View.to_buffer generate_new_buffer("*console #{dir}")
    raise "dir '#{dir}' doesn't exist" unless File.directory?(dir)
    elvar.default_directory = dir
    $el.shell current_buffer
  end

  def self.enter command=nil
    View.insert command if command

    begin
      comint_send_input
    rescue
      #       Ol << "Console.enter error here!"
    end
  end

  def self.console?
    View.mode == :shell_mode
  end

  def self.to_shell_buffer dir=nil, options={}
    if dir
      dir = "#{dir}/" unless dir =~ /\/$/
      pattern = /^\*console #{Regexp.quote(dir)}(<| |$)/
    else
      # If already in a shell (regardless of buffer name)
      return true if View.mode == :shell_mode
      pattern = /^\*console/
    end
    return true if View.name =~ pattern   # If already there, do nothing

    # Try to find an existing dir
    View.list.each do |w|
      next if buffer_name(window_buffer(w)) !~ pattern
      View.to_window(w)
      return true
    end

    if Keys.prefix_u(:clear=>true)

      found = Buffers.list.find do |b|
        name = Buffers.name b
        next false unless name =~ pattern

        view = nil
        with(:save_window_excursion) do
          View.to_buffer name

          next false unless Console.prompt?

          cd_dir = View.dir
          cd_dir = "#{cd_dir}/" unless cd_dir =~ /\/$/ 
          next false unless cd_dir == dir
          next false if Console.commands.join("\n") =~ /^(ssh|ftp) /
          true
        end
      end

      if found
        View.to_upper
        return View.to_buffer(found)
      end

    end

    # Wasn't found in visible, so don't create it if so instructed
    return false   if options[:no_create]

    if dir =~ /@/   # If there's a @, it's remote
      View.handle_bar
      View.to_buffer generate_new_buffer("*console #{dir}")
      elvar.default_directory = "/tmp"
      $el.shell current_buffer
      if dir =~ /(.+?)(\/.+)/   # Split off dir if there
        line = self.ssh_line($1)
        Console.enter line
        options[:cd_and_wait] ?
          View.insert("cd #{$2} && ") :
          Console.enter("cd #{$2}")
      else
        line = self.ssh_line(dir)
        Console.enter line
      end
    else
      Console.open dir
    end
    return true
  end

  def self.do_last_command
    #     Code.open_log_view if Keys.prefix_u

    orig = View.index

    found = self.to_shell_buffer(nil, :no_create=>true)   # If not in shell buffer, go to it

    return View.message("No *console buffer was visible") unless found

    $el.erase_buffer
    comint_previous_input(1)
    self.enter
    View.to_nth orig
  end

  def self.launch_dollar options={}
    orig = Location.new
    orig_view = View.index
    path = Tree.construct_path(:list=>true)

    path[0] = Bookmarks[path[0]] if path[0] =~ /^(\.\/|\$[\w-])/   # Expand out bookmark or ./, if there
    if path.first =~ /^\//   # If has dir (possibly remote)
      line = path.join('')
      dir, command = line.match(/(.+?)\$ (.*)/)[1..2]
      self.append_log "#{command}", dir, '$ '
      Console.to_shell_buffer dir, :cd_and_wait=>true
    else   # Otherwise, if by itself
      command = Line.without_label.match(/.*?\$ (.+)/)[1]
      self.append_log "#{command}", dir, '$ '
      Console.to_shell_buffer   # Go to shell if one is visible, and starts with "*console"
    end

    View.insert command
    Console.enter

    orig.go unless orig_view == View.index

  end

  # Mapped to !! or ! in Launcher
  def self.launch options={}
    line = Line.without_label :leave_indent=>true
    # If indented, check whether file tree, extracting if yes
    if line =~ /^\s+!/
      orig = View.cursor
      path = Tree.construct_path(:list=>true)
      if path[0] =~ /@/   # If there's a @, it's remote
        self.append_log path[1], path[0]
        return Remote.command path
      end
      if FileTree.handles?(path)
        while(path.last =~ /^!/) do   # Remove all !foo lines from path
          path.pop
        end
        dir = path.join('')
      end
      View.to orig
    end
    line =~ / *(.*?)!+ ?(.+)/
    dir ||= $1 unless $1.empty?
    command = $2

    if options[:sync]
      output = Console.run command, :dir=>dir, :sync=>true
      output.sub!(/\A\z/, "\n")   # Add linebreak if blank

      Keys.prefix == 1 ? output.gsub!(/^/, '|') : output.gsub!(/^/, '| ').gsub!(/^\| +$/, '|')

      # Expose "!" and "- label: !" lines as commands
      output.gsub!(/^\| !/, '!')
      output.gsub!(/^\| (- [\w ,-]+: !)/, "\\1")

      Tree.indent(output)
      Tree.insert_quoted_and_search output
    else
      View.handle_bar
      Console.run command, :dir=>dir  #, :buffer=>"*console #{dir}"
    end

    self.append_log command, dir, '! '

  end

  def self.append_log command, dir, prefix=''
    return if View.name =~ /_log.notes$/
    if dir.nil?
      dir ||= View.dir
      dir = "#{dir}/" if dir !~ /\/$/
    end

    command = command.dup
    command.gsub!(/^/, prefix) unless command =~ /^ *!/
    command.gsub!(/^/, '  ')

    txt = "- #{dir}\n#{command}\n"
    File.open(@@log, "a") { |f| f << txt } rescue nil
  end

  def self.ssh_line path
    path = path.sub /^\//, ''
    path.sub! /\/$/, ''

    if path =~ /(.+):(.+)/   # If port exists (colon)
      "ssh -p #{$2} #{$1}"
      # Pull out and pass with -p
    else
      "ssh -A #{path}"
    end
  end

  def self.do_as_execute options={}

    if FileTree.handles? && ! Line.matches(/^\s*\|/)   # If we're in a file tree
      path = Tree.construct_path

      #       # Run command inside of dir
      #       if Line.matches(/\/$/)   # If a dir
      #         command = Keys.input :prompt=>"Do shell command on '#{file}': "
      #         output = Console.run(command, :dir=>path, :sync=>true)
      #         FileTree.insert_under(output) if options[:insert]
      #         return View.message "Command ran with output: #{output.strip}."
      #       elsif Keys.prefix_n
      #         View.message "Running command on multiple files isn't implemented yet."
      #         return
      #       end

      file = Line.without_label
      command = Keys.input :prompt=>"Shell command on this file (_ means the filename): "
      command = command =~ /\b_\b/ ? command.gsub(/\b_\b/, "\"#{file}\"") : "#{command} \"#{file}\""

      output = Console.run(command, :dir=>File.dirname(path), :sync=>true)
      Tree.under(output, :escape=>'| ') if options[:insert]

      return View.message "Command ran with output: #{output.strip}."
    end

    command = Keys.input :prompt=>"Do shell command on '#{View.file_name}': "
    command = "#{command} #{View.file_name}"
    output = Console.run(command, :dir=>View.dir, :sync=>true)
    View.insert(output) if options[:insert]

    return View.message "Command ran with output: #{output.strip}."

  end

  # Whether buffer ends with shell prompt "...$ "
  def self.prompt?
    txt = View.txt(:left=>(View.bottom-5), :right=>View.bottom)
    txt =~ /\$ \z/
  end

  def self.history bm

    dir = Bookmarks[bm]
    dir = Files.dir_of dir

    console_log = IO.read(@@log)

    result = []
    match = false
    console_log.split("\n").each do |l|
      if l =~ /^[+-] /
        next match = l =~ /\A- #{Regexp.escape dir}/
      end

      result << "#{l}" if match
    end

    #     View.to_buffer "*commands run in #{dir}"
    #     Notes.mode
    #     View.kill_all
    #     View result
    #     View.to_bottom
    #     Search.isearch nil, :reverse=>true

    return CodeTree.tree_search_option+"- #{dir}\n"+result.reverse.uniq.join("\n")+"\n"

  end

  def self.commands
    matches = $el.elvar.comint_input_ring.to_s.scan(/#\("(.+?)" /).flatten

    matches.map!{|o| o.gsub '\\"', '"'}
    matches
  end

  def self.custom_history
    dir = View.dir
    history = Console.commands
    history.uniq! unless Keys.prefix_u
    history = history.join("\n").gsub(/^/, '$ ')
    View.create :u if ! View.list_names.member?("*shell history")
    View.to_buffer "*shell history"
    View.kill_all
    Notes.mode

    View.insert "#{history}\n"
    View.to_highest
    Tree.search
  end

  def self.search_last_commands

    bm = Keys.input(:timed => true, :prompt => "bookmark to show commands for (space for currently open): ")
    return Launcher.open("- Console.tree/") if bm == " "
    if bm == "8"
      Console.log; View.to_bottom; Search.isearch nil, :reverse=>true
      return
    end

    Launcher.open("- Console.history \"$#{bm}\"/")
  end

  def self.tree console=nil, command=nil

    if console
      View.to_buffer console.sub /\/$/, ''
      return
    end

    txt = ""

    with(:save_excursion) do

      Buffers.list.each do |b|
        next if $el.buffer_file_name b
        name = $el.buffer_name b
        next if name !~ /^\*/
        $el.set_buffer b
        next if $el.elvar.major_mode.to_s != 'shell-mode'

        next if name == "*output - tail of /tmp/output_ol.notes"

        txt << "- #{name}/\n"
        self.commands.reverse.each do |h|
          txt << "  | $ #{h}\n"
        end
      end
    end

    CodeTree.tree_search_option + txt
  end

end

Keys.custom_history(:shell_mode_map) { Console.custom_history }
