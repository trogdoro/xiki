module Xiki
  class Chmod
    def self.menu mode=nil

      path = Tree.closest_dir yield[:ancestors]

      return "| Nest this menu under a file, like...\n|\n| /tmp/\n|  @chmod/" if ! path

      if ! mode
        base_10_number = File.stat(path).mode
        mode = sprintf("%o", base_10_number)[/...$/]
        return "
          - executable) 755/
          - normal) 644/
          - only owner) 600/
          - unrestricted) 777/
          - original) #{mode}/
          "
      end

      # Arg passed, so do chmod

      command = "chmod #{mode} \"#{path}\""
      command = "sudo #{command}" # if ! File.writable? path

      Console.run command, :dir=>path

    end
  end
end
