module Command
  class Docker

    MENU = "
      + .images/
      + .containers/
    "

    def self.images image=nil, command=nil

      options=yield

      # Root, so prepend images...

      if ! image
        txt = Shell.command "docker images"
        txt.sub! /.+\n/, ''
        txt.gsub!(/ +/, ':')
        txt.gsub!(/([^:]+:[^:]+).*/, "\\1")
        return Tree.quote txt
      end

      # /image, so say to run command...

      # ~, so show tasks...

      options = yield
      task = options[:task]

      image = image.sub /^: /, ''
      command = command && command.sub(/^% /, '')

      return "~ bash session\n~ run in background" if task == [] && ! command
      return "~ grab to shell" if task == []

      # ~ foo, so run task...

      if task == ["bash session"]
        return DiffLog.quit_and_run "docker run -it #{image} bash"
      elsif task == ["run in background"]
        Shell.command "docker run -itd #{image} bash"
        return "<! started container in background"
      elsif task == ["grab to shell"]
        return DiffLog.quit_and_run "docker run -it #{image} #{command}"
      end

      # /image, so say to pass it a command...

      if ! command
        options[:line_found], options[:column_found], options[:no_search] = 2, 2, true
        return "
          | Enter a shell command to run (example: whoami):
          % 
          "
      end

      # /image/command, so say to pass it a command...

      # return args.inspect

      options[:no_slash] = 1
      Tree.quote Shell.command("docker run -t #{image} #{command}")

    end


    def self.containers container=nil #, command=nil

      options = yield
      task = options[:task]

      # /...

      if ! container

        # /~, so show tasks...

        return "~ all" if task == []

        flags = task == ["all"] ? "-a" : ""
        return Tree.quote(Shell.command("docker ps #{flags}").gsub(/ +$/, ''))
      end

      container = container.split(/ +/)
      container, tag = container[1], container[2]

      # /container/~ or /container, so show tasks...

      return "~ attach\n~ stop\n~ remove\n~ remove each" if task == [] || ! task

      # /

      if task == ["stop"]
        Shell["docker stop #{container}"]
        return "<! stopped"
      elsif task == ["remove"]
        Shell["docker rm #{container}"]
        return "<! removed"
      elsif task == ["remove each"]

        # 1. Get siblings

        siblings = Tree.siblings

        # 2. pull out ones with same name
        siblings = siblings.select{|o| o.split(/ +/)[2] == tag}
        ids = siblings.map{|o| o.split(/ +/)[1]}.join(" ")

        # 3. delete each, using a single command

        Shell["docker rm #{ids}"]

        return "<! removed each!"
      elsif task == ["attach"]
        return DiffLog.quit_and_run "docker attach #{container}"
      end

    end

  end
end
