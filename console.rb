require 'open3'

class Console
  extend ElMixin

  CODE_SAMPLES = %q[
    # Run OS commands
    - in console (asynchronously): Console.run("ls", :dir => "/tmp")
    - Synchronously: Console.run("ls", :dir => "/etc", :sync => true)
  ]

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
      stdin, stdout, stderr = Open3.popen3("#{profile}cd #{dir};#{command}")
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

      if ! reuse_buffer# || ! View.buffer_open?(buffer)
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
        #comint_send_input
      end
    end
  end

  def self.open dir=nil
    View.handle_bar
    dir ||= elvar.default_directory
    View.to_buffer generate_new_buffer("*console #{dir}")
    elvar.default_directory = dir
    $el.shell current_buffer
  end

  def self.enter command=nil
    View.insert command if command
    comint_send_input
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
        Console.enter "cd #{$2}"
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
    # TODO if none visible, don't do anything
    found = self.to_shell_buffer(nil, :no_create=>true)   # If not in shell buffer, go to it

    return View.message("No *console buffer was visible") unless found

    $el.erase_buffer
    comint_previous_input(1)
    self.enter
  end

  def self.launch_dollar options={}
    orig = Location.new
    orig_view = View.index
    path = FileTree.construct_path(:list=>true)
    path[0] = Bookmarks[path[0]] if path[0] =~ /^\$[\w-]/   # Expand out bookmark at root, if there
    if path.first =~ /^\//   # If has dir (possibly remote)
      line = path.join('')
      dir, command = line.match(/(.+?)\$ (.+)/)[1..2]
      Console.to_shell_buffer dir
    else   # Otherwise, if by itself
      command = Line.without_label.match(/.*?\$ (.+)/)[1]
      Console.to_shell_buffer   # Go to shell if one is visible, and starts with "*console"
    end

    View.insert command
    Console.enter

    orig.go unless orig_view == View.index
  end

  # Mapped to !! or ! in LineLauncher
  def self.launch options={}
    line = Line.without_label :leave_indent=>true
    #p Line.without_label
    # If indented, check whether file tree, extracting if yes
    if Line.value =~ /^\s+!/
      orig = View.cursor
      # - of previous line
      path = FileTree.construct_path(:list=>true)
      if FileTree.handles?(path)
        # Remove all !foo lines
        while(path.last =~ /^!/)
          path.pop
        end

        dir = path.join('')
      end
      View.to orig
    end
    line =~ / *(.*?)!+(.+)/
    dir ||= $1 unless $1.blank?

    command = $2
    if options[:sync]
      output = Console.run command, :dir=>dir, :sync=>true
      output.sub!(/\A\z/, "\n")   # Add linebreak if blank
      output.gsub!(/^/, '!')
      FileTree.indent(output)
      FileTree.insert_quoted_and_search output
    else
      View.handle_bar
      Console.run command, :dir=>dir#, :buffer=>"*console #{dir}"
    end
  end

  def self.ssh_line path
    path.sub! /^\//, ''
    path.sub! /\/$/, ''

    if path =~ /(.+):(.+)/   # If port exists (colon)
      "ssh -p #{$2} #{$1}"
      # Pull out and pass with -p
    else
      "ssh #{path}"
    end
  end

end
