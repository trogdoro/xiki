module Xiki

  # This just has the consolidated (post-Unified) methods that
  # are used by key shortcuts (since methods in menu/git.rb might
  # not be loaded yet).
  class Git

    class << self
      attr_accessor :line_found
    end

    def self.status_to_hash txt

      # txt example:
      #   AM a.txt
      #    M committed.txt
      #   A  d/d.txt
      #    D deleteme.txt
      #   ?? e.txt

      result = {}

      # Pull out lines like these to :untracked...
      #   ?? a.txt

      untracked = txt.scan(/^(\?\?) (.+)/)
      untracked.each{|o| o[0] = "untracked" }
      result[:untracked] = untracked

      # Pull out lines like these to :unadded...
      #   AM a.txt
      #    M committed.txt
      #    D deleteme.txt

      unadded = txt.scan(/^.(\w) (.+)/)
      unadded.each{|o| o[0] = {"M"=>"modified", "D"=>"deleted"}[o[0]] }
      result[:unadded] = unadded

      # Pull out lines like these to :added...
      #   AM a.txt
      #   A  d/d.txt
      #   R  rename.txt -> renamed.txt

      added = txt.scan(/^(\w). (.+)/)

      added.each{|o| o[0] = {"A"=>"new file", "R"=>"renamed"}[o[0]] }
      result[:added] = added

      # result example:
      #   :unadded   => [
      #     ["modified", "a.txt"],
      #     ["deleted", "deleteme.txt"],
      #   ],
      #   :added     => [
      #     ["new file", "a.txt"],
      #     ["renamed", "rename.txt -> renamed.txt"]
      #   ],
      #   :untracked => [
      #     ["untracked", "e.txt"]
      #   ]

      result

    end

    def self.branch_name dir=nil

      dir ||= Tree.closest_dir

      branch, error = Shell.command "git rev-parse --abbrev-ref HEAD", :dir=>Files.dir_of(dir), :return_error=>1

      # Error with "unknown revision" means no revisions yet, so return branch as MASTER...
      return "no commits yet?" if error =~ /^fatal: ambiguous argument 'HEAD': unknown revision/

      # Unknown error, so return branch as nil...
      return nil if error

      # No error, so return branch...
      branch.strip

    end

    def self.do_push
      prefix = Keys.prefix :clear=>true

      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to git diff in: "
      branch = self.branch_name dir
      txt = "
        #{dir}
          $ git
            - push/#{branch}/
            - diff/
        ".unindent

      if prefix == :-
        View.insert txt
        Line.previous
        Launcher.launch
      else
        View.bar if prefix == "outline"
        Launcher.open txt, :buffer_name=>"git diff/", :buffer_dir=>dir
      end

      nil
    end


    def self.do_log

      prefix = Keys.prefix :clear=>1

      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to git diff in: "

      txt = "
        #{dir}
          $ git log
        ".unindent

      Launcher.open txt, :buffer_name=>"git log"

      nil
    end

    def self.do_status

      prefix = Keys.prefix :clear=>1
      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to git diff in: "

      txt = "
        #{dir}
          $ git status
        ".unindent

      Launcher.open txt, :buffer_name=>"git log"

      nil
    end


    def self.do_compare_repository
      file = View.file

      Launcher.open "#{file}\n  = git/diff/", :line_found=>View.line

      ""
    end


    def self.toplevel_split path
      dir = Shell.sync "git rev-parse --show-toplevel", :dir=>Files.dir_of(path)

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
