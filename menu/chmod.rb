module Xiki
  class Chmod
    def self.menu mode=nil

      path = Tree.file :require=>1

      if ! mode
        base_10_number = File.stat(path).mode
        mode = sprintf("%o", base_10_number)[/...$/]
        return "
          - original) #{mode}/
          > Suggestions
          - no restrictions) 777/
          - normal) 644/
          - executable) 755/
          - only owner) 600/
          "
      end

      # Arg passed, so do chmod

      command = "chmod #{mode} \"#{path}\""
      command = "sudo #{command}" # if ! File.writable? path

      Console.run command, :dir=>path

    end
  end
end
