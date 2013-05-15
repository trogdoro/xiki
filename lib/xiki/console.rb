require 'open3'

class Console

  @@log = File.expand_path("~/.emacs.d/console_log.notes")

  def self.menu
    # All of this is deprecated, I think?
    %`
    - .log/
    - .tree/
    - .history/
    - api/
      > In console (asynchronously)
      @Console.run "ls"
      @Console.run "ls", :dir=>"/tmp"

      > Inline (synchronously)
      @Console.sync "ls"
      @Console.sync "ls", :dir=>"/etc"
    - docs/
      You can run shell commands by typing things like this...

      > In current dir
      @ $ ls

      > In other dir
      @ /tmp/
        $ ls

      > Async, in any open console view
      @ /tmp/
        % ls

      > Async, in other dir
      @ /tmp/
        % ls

      > Async, in iTerm
      @ /tmp/
        & ls

      > Commands you've run recently
      << log/

      > Commands from currently open consoles
      << tree/
    `
  end

  def self.log
    View.open @@log
  end

  # Run the command in a console
  def self.[] command
    self.run command, :sync=>1
  end

  def self.sync command, options={}
    self.run command, options.merge(:sync=>1)
  end

  #
  # Runs shell command asynchronously.
  #
  # Console.run "ls"
  # Console.run "ls", :dir=>"/tmp/"
  #
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
      dir = "#{$el.elvar.default_directory}/#{dir}" unless dir =~ /^\//
      dir = dir.gsub(/\/\/+/, '/')

      # If file, but not dir, try backing up to the dir
      raise "- Directory '#{dir}' doesn't exist!" if ! File.exists? dir

      dir.sub!(/[^\/]+$/, '') if ! File.directory?(dir)

      # If dir exists, continue
      if File.directory?(dir)
        # Put slash on end if not there
        dir = "#{dir}/" unless dir =~ /\/$/
      else  # Otherwise, exit
        return puts("#{dir} is not a dir")
      end
    else
      dir = $el ? $el.elvar.default_directory : "/tmp/"
    end

    if sync

      return command if options[:no_enter]
      profile = File.exists?(File.expand_path('~/.profile')) ? '. ~/.profile;' : ''
      stdin, stdout, stderr = Open3.popen3("#{profile}cd \"#{dir}\";#{command}")

      if txt = options[:stdin]
        stdin.puts txt
        stdin.close
      end

      result = ""
      result << stdout.readlines.join('')
      result << stderr.readlines.join('')

      result.force_encoding("binary") if result.respond_to? :force_encoding
      result.gsub!("\c@", '.')   # Replace out characters that el4r can't handle
      return result

    else
      if View.in_bar? and ! options[:dont_leave_bar]
        View.to_after_bar
      end
      buffer ||= "*console #{dir}"

      if ! reuse_buffer
        buffer = $el.generate_new_buffer(buffer)
      end
      View.to_buffer buffer
      $el.erase_buffer if reuse_buffer
      $el.elvar.default_directory = dir if dir
      $el.shell $el.current_buffer

      # Don't prompt with "buffer has a running process" when closing
      $el.set_process_query_on_exit_flag $el.get_buffer_process($el.current_buffer), nil

      Move.bottom
      if command  # If nil, just open console
        $el.insert command
        Console.enter unless options[:no_enter]
      end
    end

    nil
  end

  def self.open dir=nil
    View.handle_bar
    dir ||= $el.elvar.default_directory
    dir = File.expand_path(dir)+"/"
    View.to_buffer $el.generate_new_buffer("shell")
    raise "dir '#{dir}' doesn't exist" unless File.directory?(dir)
    $el.elvar.default_directory = dir
    $el.shell $el.current_buffer
  end

  def self.enter command=nil
    View.insert command if command

    begin
      $el.comint_send_input
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

    if ! dir

      # Try to find visible shell buffer in same dir and with prompt

      View.list.each do |w|
        $el.set_buffer $el.window_buffer(w)
        next if View.mode != :shell_mode || ! Console.prompt?
        next if View.cursor != View.bottom
        View.to_window(w)
        return true
      end
    end

    if dir && dir !~ /@/   # If local dir

      # TODO Make sure there's no ssh or cd in history!
      View.list.each do |w|
        $el.set_buffer $el.window_buffer(w)
        next if View.mode != :shell_mode || ! Console.prompt?
        next if Tree.slashless(dir) != Tree.slashless(View.dir)
        next if View.cursor != View.bottom

        View.to_window(w)
        return true
      end

    # TODO: implement similar finding a dir for remote
      # else

    end

    # Deprecated:
    # Try to find visible shell buffer with matching name
    View.list.each do |w|
      next if $el.buffer_name($el.window_buffer w) !~ pattern
      View.to_window(w)
      return true
    end

    if Keys.prefix_u(:clear=>true)

      found = Buffers.list.find do |b|
        name = Buffers.name b
        next false unless name =~ pattern

        view = nil
        $el.with(:save_window_excursion) do
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

    # Wasn't found among visible, so create new buffer

    return false   if options[:no_create]   # Don't create it if option says not to

    if dir =~ /@/   # If there's a @, it's remote
      View.handle_bar
      View.to_buffer $el.generate_new_buffer("*console #{dir}")
      $el.elvar.default_directory = "/tmp"
      $el.shell $el.current_buffer
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
    $el.comint_previous_input(1)
    self.enter
    View.to_nth orig
  end

  def self.launch_async options={}

    orig = Location.new
    orig_view = View.index
    path = Tree.construct_path(:list=>true)

    path[0] = Bookmarks[path[0]] if path[0] =~ /^(\.\/|\$[\w-])/   # Expand out bookmark or ./, if there
    if path[0] =~ /^\//   # If has dir (local or remote)
      line = path.join('')
      dir, command = line.match(/(.+?)[%&] (.*)/)[1..2]
      self.append_log "#{command}", dir, '% '

      return Iterm.run("cd #{dir}\n#{command}", :activate=>1) if Keys.prefix_uu
      return Iterm.run("cd #{dir}\n#{command}") if Keys.prefix_u || options[:iterm]

      Console.to_shell_buffer dir, :cd_and_wait=>true
    else   # Otherwise, if by itself - meaning on own line?
      command = Line.without_label.match(/.*?[%&] ?(.*)/)[1]
      self.append_log("#{command}", dir, '% ') if command.present?

      return Iterm.run("#{command}", :activate=>1) if Keys.prefix_uu
      return Iterm.run("#{command}", :activate=>1) if Keys.prefix_u && options[:iterm]
      return Iterm.run("#{command}") if Keys.prefix_u || options[:iterm]

      self.to_shell_buffer   # Go to shell if one is visible
    end
    return if command.empty?

    View.to_bottom

    View.insert command
    Console.enter

    orig.go unless orig_view == View.index
  end

  # Synchronous - mapped to $ launcher
  def self.launch options={}
    trunk = Xiki.trunk   # use Tree.path instead

    # If it looks like error output, just jump to it
    if trunk[-1] =~ /[^\/+]\/\| \s+from / || trunk[-1] =~ /[^\/+]\/\| +\//
      return Code.open_as_file
    end

    # if not under file
    # raise RelinquishException.new

    # Run in current dir if no parent or @$
    if trunk[-1] =~ /^\$ /
      # TODO Run in current dir?
      # return
    end

    # There's a dir in our chunk, so relinquish control if not fire tree

    # Handle if
      # no parent in our chunk
      # parent in our chunk is file tree

    line = Line.without_label :leave_indent=>true
    # If indented, check whether file tree, extracting if yes
    if line =~ /^\s+\$/
      orig = View.cursor
      path = Tree.construct_path(:list=>true)
      if path[0] =~ /@/   # If there's a @, it's remote
        self.append_log path[1], path[0]
        return Remote.command path
      end
      if FileTree.handles?(path)
        while(path.last =~ /^\$ /) do   # Remove all $ foo lines from path
          path.pop
        end
        dir = path.join('')

        # If starts with ./, replace with current dir
        dir.sub! /^\.\//, "#{View.dir}/"

        dir = Bookmarks[dir]

      end
      View.to orig
    end
    line =~ / *@? ?(.*?)\$+ ?(.+)/
    dir ||= $1 unless $1.empty?
    command = $2

    return Tree.<<("- Directory doesn't exist) #{dir}", :no_slash=>1) if dir && ! File.exists?(dir)

    # Run command...

    if options[:sync]
      output = Console.run command, :dir=>dir, :sync=>true
      output.sub!(/\A\z/, "\n")   # Add linebreak if blank

      # Clean up ^H formatting
      output.gsub!(/.\cH/, "")   # Add linebreak if blank

      Keys.prefix == 0 ? output.gsub!(/^/, '|') : output.gsub!(/^/, '| ')

      output.gsub!(/^\| +$/, '|')

      output.sub! /\n*\z/, "\n"   # Guarantee exactly 1 linebreak at end
      Tree.indent(output)

      Tree.insert_quoted_and_search output
    else
      View.handle_bar
      Console.run command, :dir=>dir  #, :buffer=>"*console #{dir}"
    end

    self.append_log command, dir, '$ '

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
    right = View.bottom
    left = right - 10
    left = 1 if left < 1
    txt = View.txt left, right

    txt =~ /[>#%$] \z/
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

    "@#{dir}\n"+result.reverse.uniq.join("\n")+"\n"

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
    history = history.join("\n").gsub(/^/, '% ')
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
    return Launcher.open("console/tree/") if bm == " "
    if bm == "8"
      Console.log; View.to_bottom; Search.isearch nil, :reverse=>true
      return
    end

    Launcher.open("console/history/$#{bm}/")
  end

  def self.tree *args
    command = args.pop if args[-1] =~ /^\|/
    console = args.any? ? args.join("/") : nil

    if console
      View.to_buffer console#.sub /\/$/, ''
      return
    end

    txt = ""

    $el.with(:save_excursion) do

      Buffers.list.each do |b|
        next if $el.buffer_file_name b
        name = $el.buffer_name b
        $el.set_buffer b
        next if $el.elvar.major_mode.to_s != 'shell-mode'

        next if name == "*ol"

        txt << "- #{name}/\n"
        self.commands.reverse.each do |h|
          txt << "  | $ #{h}\n"
        end
      end
    end

    txt
  end

  def self.exit   # Kills running server or process in shell
    $el.comint_interrupt_subjob
  end

  def self.wait_until buffer, options={}
    max = options[:max] || 10
    message = options[:message] || "Launching..."
    while View.txt(:buffer=>buffer) !~ options[:contains]
      View.flash message, :times=>1
      max -= 1
      break if max < 0
    end
  end

  def self.shell_command_per_prompt prompt, options
    dir, command = options[:dir], options[:command]

    self.append_log command, dir, "#{prompt} "

    case prompt
    when "$"
      txt = Tree.quote Console.sync command, :dir=>dir
      txt.gsub!(/^\| /, "|") if options[:prefix] == 0
      return txt
    when "%"
      return Console.run command, :dir=>dir
    when "&"
      return Iterm.run("cd #{dir}\n#{command}")
    end
  end

end

Keys.custom_history(:shell_mode_map) { Console.custom_history }
