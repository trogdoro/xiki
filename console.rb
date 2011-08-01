require 'open3'

class Console
  extend ElMixin

  CODE_SAMPLES = %q[
    # Run OS commands
    - in console (asynchronously): Console.run("ls", :dir => "/tmp")
    - Synchronously: Console.run("ls", :dir => "/etc", :sync => true)
  ]

  @@log = File.expand_path("~/.emacs.d/console_log.notes")

  def self.menu
    ['.log']
  end

  def self.log
    View.open @@log
  end

  # Run the command in a console
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
      Ol << "Console.enter error here!"
    end
  end

  def self.console?
    View.mode == :shell_mode
  end

  def self.to_shell_buffer dir=nil, options={}

    if dir
      dir = "#{dir}/" unless dir =~ /\/$/
      pattern = /^\*console #{Regexp.quote(dir)}(<|$)/
    else
      # If already in a shell (regardless of buffer name)
      return true if View.mode == :shell_mode
      pattern = /^\*console/
    end
    return true if View.name =~ pattern   # If already there, do nothing

    w = View.list.find{|w| buffer_name(window_buffer(w)) =~ pattern}
    if w   # If found, just go to it
      View.to_window(w)
      return true
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
    Code.open_log_view if Keys.prefix_u

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
    path = FileTree.construct_path(:list=>true)

    path[0] = Bookmarks[path[0]] if path[0] =~ /^(\.\/|\$[\w-])/   # Expand out bookmark or ./, if there
    if path.first =~ /^\//   # If has dir (possibly remote)
      line = path.join('')
      dir, command = line.match(/(.+?)\$ (.*)/)[1..2]
      Console.to_shell_buffer dir, :cd_and_wait=>true
    else   # Otherwise, if by itself
      command = Line.without_label.match(/.*?\$ (.+)/)[1]
      Console.to_shell_buffer   # Go to shell if one is visible, and starts with "*console"
    end

    View.insert command
    Console.enter

    self.append_log "#{command}", dir, '$ '

    orig.go unless orig_view == View.index
  end

  # Mapped to !! or ! in LineLauncher
  def self.launch options={}
    line = Line.without_label :leave_indent=>true
    # If indented, check whether file tree, extracting if yes
    if line =~ /^\s+!/
      orig = View.cursor
      path = FileTree.construct_path(:list=>true)
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

      output.gsub!(/^/, '| ')

      # Expose "!" and "- label: !" lines as commands
      output.gsub!(/^\| !/, '!')
      output.gsub!(/^\| (- [\w ,-]+: !)/, "\\1")

      FileTree.indent(output)
      FileTree.insert_quoted_and_search output
    else
      View.handle_bar
      Console.run command, :dir=>dir  #, :buffer=>"*console #{dir}"
    end

    self.append_log command, dir, '! '

  end

  def self.append_log command, dir, prefix=''
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
      "ssh #{path}"
    end
  end

  def self.insert_command

    bm = Keys.input(:timed => true, :prompt => "Enter bookmark for dir to insert command: ")
    dir = Bookmarks.expand bm, :just_bookmark=>true
    dir = "#{File.expand_path(dir)}"

    prompt = Keys.prefix_u ? '!' : '$'

    View.insert "- #{dir}/\n  #{prompt} "
  end

  def self.do_as_execute options={}

    if FileTree.handles? && ! Line.matches(/^\s*\|/)   # If we're in a file tree
      path = FileTree.construct_path

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
      FileTree.insert_under(output) if options[:insert]

      return View.message "Command ran with output: #{output.strip}."
    end

    path = View.dir

    command = Keys.input :prompt=>"Do shell command in '#{path}': "
    output = Console.run(command, :dir=>path, :sync=>true)
    View.insert(output) if options[:insert]

    return View.message "Command ran with output: #{output.strip}."

  end
end
