class Shell
  extend ElMixin

  CODE_SAMPLES = %q[
    # Run OS commands
    - in shell (asynchronously): Shell.run("ls", :dir => "/tmp")
    - Synchronously: Shell.run("ls", :dir => "/etc", :sync => true)
  ]

  # Run the command in a shell
  def self.run command, options={}
    #dir=nil, sync=nil, buffer_name=nil
    dir = options[:dir]
    sync = options[:sync]
    buffer_name = options[:buffer]

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
      stdin, stdout, stderr = Open3.popen3(". ~/.profile;cd #{dir};#{command}")
      result = ""
      result << stderr.readlines.join('')
      result << stdout.readlines.join('')
      return result

    else
      if View.in_bar?
        View.to_after_bar
      end
      buffer_name ||= "*shell*"
      switch_to_buffer generate_new_buffer(buffer_name)
      elvar.default_directory = dir if dir
      shell current_buffer
      Move.bottom
      if command  # If nil, just open shell
        insert command
        Shell.enter
        #comint_send_input
      end
    end
  end

  def self.open
    control_lock_enable if elvar.control_lock_mode_p
    dir = elvar.default_directory
    switch_to_buffer generate_new_buffer("*shell*")
    elvar.default_directory = dir
    shell current_buffer
  end

  def self.enter
    #command_execute
    comint_send_input
  end

  def self.shell?
    elvar.major_mode.to_s == "shell-mode"
  end

  def self.do_last_command
    erase_buffer
    comint_previous_input(1)
    comint_send_input
  end

end
