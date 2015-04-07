module Command
  class Docker

    MENU = "
      - add image/
        ubuntu

        | Change the image name and expand, or search for one:
        + search/
    "

    def self.menu_after output, *args

      # Root, so prepend images...

      if args == []
        txt = Shell.command "docker images"
        txt.sub! /.+\n/, ''
        txt.gsub!(/ +/, ':')
        txt.gsub!(/([^:]+:[^:]+).*/, "\\1")
        return Tree.quote(txt) + output
      end

      # MENU handled it, so don't interject...

      return if output

      # /image, so say to run command...

      # ~, so show tasks...

      options = yield
      task = options[:task]

      image = args[0].sub /^: /, ''
      command = args[1] && args[1].sub(/^% /, '')

      return "~ bash session" if task == [] && ! command
      return "~ grab to shell" if task == []

      # ~ foo, so run task...

      if task == ["bash session"]
        return DiffLog.quit_and_run "docker run -t -i #{image} bash"
      end

      if task == ["grab to shell"]
        return DiffLog.quit_and_run "docker run -t -i #{image} #{command}"
      end


      # /image, so say to pass it a command...

      if args.length == 1
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

  end
end
