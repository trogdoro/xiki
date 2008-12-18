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
      buffer ||= "*console*"

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

  def self.open
    ControlLock.disable
    dir = elvar.default_directory
    switch_to_buffer generate_new_buffer("*console*")
    elvar.default_directory = dir
    $el.shell current_buffer
  end

  def self.enter
    #command_execute
    comint_send_input
  end

  def self.console?
    View.mode == :shell_mode
  end

  def self.do_last_command
    # If not in shell buffer, go to it
    if View.mode != :shell_mode
      buffer = nil
      with(:save_window_excursion) do
        buffer = buffer_list.to_a.find{|b|
          set_buffer b
          (View.mode == :shell_mode && View.name !~ /^\*output /)
        }
      end
      View.to_after_bar
      View.to_buffer buffer_name(buffer)
    end

    erase_buffer
    comint_previous_input(1)
    comint_send_input
  end

  # Mapped to !! or ! in LineLauncher
  def self.launch options={}
    line = Line.without_label :leave_indent=>true
    p Line.without_label
    # If indented, check whether code tree, extracting if yes
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
    dir ||= $1
    command = $2
    if options[:sync]
      output = Console.run command, :dir=>dir, :sync=>true
      # Add linebreak if blank
      output.sub!(/\A\z/, "\n")
      output.gsub!(/^/, '!')
      FileTree.indent(output)
      FileTree.insert_quoted_and_search output
    else
      View.handle_bar
      Console.run command, :dir=>dir, :buffer=>"*shell #{command.gsub(/[^\w]+/, ' ')[0..9]}"
    end
  end
end
