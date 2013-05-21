module Xiki
  class Chown
    def self.menu user_group=nil

      path = Tree.file :require=>1

      if ! user_group

        user = Etc.getpwuid(File.stat(path).uid).name
        group = Etc.getgrgid(File.stat(path).gid).name
        user_group = "#{user}:#{group}"

        default_user = `id -un`.strip
        default_group = `id -gn`.strip

        return "
          - original) #{user_group}/
          > Suggestions:
          - #{default_user}:#{default_group}/
          - root:wheel/
          "
      end

      # Arg passed, so do chown

      command = "chown #{user_group} \"#{path}\""
      command = "sudo #{command}"   # Trickier with user, so just always sudo for now

      Console.run command, :dir=>path

    end

  end
end
