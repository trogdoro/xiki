module Xiki

  # This just has the consolidated (post-Unified) methods that
  # are used by key shortcuts (since methods in menu/git.rb might
  # not be loaded yet).
  class Git

    class << self
      attr_accessor :line_found
    end

    def self.branch_name dir=nil
      dir ||= Tree.closest_dir
      Shell.run("git status", :sync=>true, :dir=>dir)[/# On branch (.+)/, 1]
    end

    def self.do_push
      prefix = Keys.prefix :clear=>true

      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to git diff in: "
      branch = self.branch_name dir
      txt = "
        #{dir}
          =git/
            - push/#{branch}/
            - diff/
        ".unindent

      if prefix == :-
        View.insert txt
        Line.previous
        Launcher.launch
      else
        View.bar if prefix == "outline"
        Launcher.open txt, :buffer_name=>"git diff", :buffer_dir=>dir
      end

      nil
    end


    def self.do_compare_repository
      file = View.file

      Launcher.open "#{file}\n  =git/diff/", :line_found=>View.line

      ""
    end


    def self.toplevel_split path

      dir = Shell.sync "git rev-parse --show-toplevel", :dir=>path
      return nil if dir =~ /^fatal: /

      dir.strip!
      # Fix for bullshit mac /tmp issue
      dir.sub! /^\/private\/tmp\//, "/tmp/"

      relative = path.sub /#{Regexp.escape dir}\/?/, ""

      relative = nil if relative == ""

      [dir, relative]
    end

    # Massages output of "git diff" or "git show" into :... quotes.
    # Git diff args should be like this? > --oneline -U1
    def self.format_as_quote txt, options={}

      # Remove junk at beginning before leading diff
      txt.sub!(/.*?^@@ /m, '@@ ') if options[:one_file]

      txt.gsub!(/^index .+\n/, '')
      txt.gsub!(/^diff .+\n/, '')
      txt.gsub!(/^--- .+\n/, '')
      txt.gsub!(/^\+\+\+ /, '@@ ')

      txt.gsub!(/^/, ':')
      txt.gsub!(/ $/, '')
      txt
    end

    def self.do_status
      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to show git status: "

      branch = Xiki::Git.branch_name dir
      menu = "
        #{dir}
          $ git
            + push/#{branch}/
            + status/
      ".unindent.strip
      Launcher.open menu, :buffer_dir=>dir

      nil
    end


  end
end
