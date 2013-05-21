module Xiki
  # This just has the consolidated (post-Unified) methods that
  # are used by key shortcuts (since methods in menu/git.rb might
  # not be loaded yet).
  class Git

    class << self
      attr_accessor :jump_line_number
    end

    def self.branch_name dir=nil
      dir ||= Tree.closest_dir
      Console.run("git status", :sync=>true, :dir=>dir)[/# On branch (.+)/, 1]
    end

    def self.do_push
      prefix = Keys.prefix :clear=>true

      file = Keys.bookmark_as_path :prompt=>"Enter a bookmark to git diff in: "
      branch = self.branch_name file

      menu = "
        #{file}
          @git/
            - push/#{branch}/
            - diff/
        ".unindent

      if prefix == :-
        View.insert menu
        Line.previous
        Launcher.launch_unified
      else
        View.bar if prefix == "outline"
        Launcher.open(menu)
      end

      nil
    end


    def self.do_compare_repository
      file = View.file

      # Temporarily set the line number in a temporary place
      Git.jump_line_number = View.line
      Launcher.open "#{file}\n  @git/diff/"
      Git.jump_line_number = nil

      ""
    end


    def self.toplevel_split path

      dir = Console.sync "git rev-parse --show-toplevel", :dir=>path
      return nil if dir =~ /^fatal: /

      dir.strip!
      # Fix for bullshit mac /tmp issue
      dir.sub! /^\/private\/tmp\//, "/tmp/"

      relative = path.sub /#{Regexp.escape dir}\/?/, ""

      relative = nil if relative == ""

      [dir, relative]
    end

  end
end
