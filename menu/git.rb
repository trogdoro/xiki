module Xiki::Menu
  class Git

    @@git_diff_options = '-U2'

    MENU = "
      - .diff/
      - .log/
      - .status/
      - .graph/
      - .setup/
        - .create/
        - .make sample files/
        - github/
          - Make this run in first dir it finds above!
          @% git remote add origin git@github.com:myusername/foo.git
      - .docs/
      "

    MENU_HIDDEN = "
      - .push/
      - .diff/
        - .commit/
        - .add/
        - .delete/
        - .revert/
        - .unadd/
      "

    def self.menu_before *args

      return if ['docs', 'do push'].member?(args[0])

      options = yield

      dir = Tree.closest_dir options[:ancestors]

      # Tell them to nest under something if not nested...
      return self.docs if ! dir

      exists, kind = FileTree.examine dir   # => [false, :file]

      # If file/dir doesn't exist, suggest creating

      if ! exists
        return "| File '#{dir}' doesn't exist" if kind == :file
        return FileTree.suggest_mkdir dir
      end

      branch = Xiki::Git.branch_name dir

      # If not a repo, suggest creating one...

      if args[0] != "setup" && branch.nil?
        return "| Not a git repository.  Create a new one here?\n- setup/create/"
      end

      nil
    end

    def self.menu_after output, *args
      return if args.any?

      # If /, add push/thebranch/ to the beginning

      branch = Xiki::Git.branch_name
      "+ push/#{branch}/\n#{output}"
    end

    def self.diff *args
      options = yield
      options[:no_search] = 1

      return "@prompt/Type a commit message." if args == ["commit"]

      options[:no_slash] = 1

      dir = Tree.closest_dir options[:ancestors]

      quote = args.pop if args[-1] =~ /^\|/
      path = args.any? ? args.join("/") : nil
      quote = quote.sub(/^\|/, '') if quote

      # If we're nested under a file, break up into parts
      if File.file? dir
        dir, path = Xiki::Git.toplevel_split dir
      end

      self.git_diff dir, path, quote, options
    end


    def self.do_status
      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to show git status: "

      menu = "
        #{dir}
          - @git/
            + status/
      ".unindent.strip
      Launcher.open(menu)

      nil
    end

    def self.status *args
      self.diff({:expand=>false}, *args)
    end

    def self.status_raw
      dir = Tree.closest_dir yield[:ancestors]

      result = Console.sync "echo 'TODO - finish .status_raw - #{dir}'"  #, :dir=>Dir.pwd______
      Tree.quote result
    end

    def self.make_sample_files
      dir = Tree.closest_dir yield[:ancestors]

      Dir.mkdir "#{dir}/d" rescue nil

      txt = "hello\nhi again\n"
      ["a.txt", "b.txt", "d/aa.txt"].each { |path| File.open("#{dir}/#{path}", "w") { |f| f << txt } }

      "
      | Created these files:
      | - a.txt
      | - b.txt
      | - d/
      |   - aa.txt
      "
    end

    def self.if_not_repository branch
      return nil if branch   # Fine if there's a branch

      Xiki.quote "
        > Not a repository
        This dir isn't a git repository.
        |
        > Create a repository?
        - setup/create/
        "
    end

    def self.push branch

      # If no branch, use default
      if branch.nil?
        return "- choose the branch!"
      end

      dir = Tree.closest_dir yield[:ancestors]

      self.push_internal branch, dir

      nil
    end

    def self.add
      dir = Tree.closest_dir yield[:ancestors]

      siblings = Tree.siblings

      siblings = self.remove_options siblings
      return "- No files to add (they should be siblings of add)!" unless siblings.any?

      command = "git add #{siblings.join("\\\n  ")}"

      txt = Console.sync command, :dir=>dir
      return Tree.quote(txt) if txt.any?
      "@flash/- added!"
    end

    def self.commit message=nil
      return View.prompt "Enter a commit message" if message.nil?

      dir = Tree.closest_dir(yield[:ancestors])

      self.commit_internal message, dir
    end

    def self.methods_by_date path
      txt = Console.sync "git blame \"#{path}\""
      txt = txt.split "\n"

      txt = txt.select{|o| o =~ /\) *def /}   # Remove all but method definitions
      txt.sort!{|a, b| a[/....-..-.. ..:..:../] <=> b[/....-..-.. ..:..:../]}   # Sort by date
      txt.each{|o| o.sub! /.+?\) /, ''}
      txt = txt.reverse
    end



    # Moved over from gito.rb - some of this can probably be deleted...



    def self.diff_internal command, dir
      txt = Console.sync command, :dir=>dir

      if Keys.prefix_u
        txt.gsub!(/\c[\[31m(.*?)\c[\[m/, "\(\-\\1\-\)")
        txt.gsub!(/\c[\[32m(.*?)\c[\[m/, "\(\+\\1\+\)")
        txt.gsub!(/\c[\[\d*m/, '')
        txt.gsub!("\-\)\(\-", '')   # Merge adjacent areas
        txt.gsub!("\+\)\(\+", '')
        txt.gsub!(/^./, " \\0")   # Add space at beginning of all non-blank lines
        txt.gsub!(/^ @/, '@')
        # Find whole lines
        txt.gsub!(/^ \(\+(.*)\+\)$/) {|m| $1.index("\(\+") ? m : "+#{$1}" }
        txt.gsub!(/^ \(\-(.*)\-\)$/) {|m| $1.index("\(\-") ? m : "-#{$1}" }
        # Remove empty (--)'s
        txt.gsub! /\([+-][+-]\)/, ''
      else
        txt.gsub! /^ $/, ''
      end

      txt
    end

    def self.status_internal txt
      txt.gsub!(/^#\t(.+?): +/, "- \\1: ")
      txt.gsub!(/^#\t/, "- ")
      txt.gsub!(/^#\n/, '')
      txt.gsub!(/^#/, '|')
      txt.gsub! /.+ \.\..+\n/, ""
      txt
    end

    # Takes as input the output of .status_internal.
    def self.status_to_hash txt
      result = {}

      # Pull out unadded
      unadded = txt[/^\| Changed but not updated:.+/m]
      result[:unadded] =
        if unadded
          unadded.sub! /\A(^\|[^\n]+\n)+/m, ''   # Remove first few headings
          unadded.sub! /^\|.+/m, ''   # Remove future sections
          unadded.scan(/^- (.+?): (.+)/)
        else
          []
        end

      # Pull out added
      added = txt[/^\| Changes to be committed:.+/m]
      result[:added] =
        if added
          added.sub! /\A(^\|[^\n]+\n)+/m, ''   # Remove first few headings
          added.sub! /^\|.+/m, ''   # Remove future sections
          added.scan(/^- (.+?): (.+)/)
        else
          []
        end

      # Pull out untracked
      untracked = txt[/^\| Untracked files:.+/m]
      result[:untracked] =
        if untracked
          files = untracked.scan(/^- (.+)/)
          files.map!{|i| ['untracked', i[0]]}
        else
          []
        end

      result
    end


    def self.graph *args
      dir = Tree.closest_dir(yield[:ancestors])

      txt = Console.sync %`git log --graph --full-history --all --pretty=format:"%h%x09%d%x20%s"`, :dir=>dir

      Tree.quote txt
    end

    def self.log *args

      options = yield

      dir = Tree.closest_dir(options[:ancestors])

      toplevel, relative = Xiki::Git.toplevel_split dir

      rev, *file_and_line = args

      # search=nil, project=nil, rev=nil, *file_and_line

      prefix = options[:prefix]

      # /, so list all revs...

      if rev.nil?

        #         command = "git log -1000 --pretty=oneline"
        command = "git log -1000 --oneline"
        command << " '#{relative}'" if relative

        txt = Console.run command, :sync=>true, :dir=>toplevel

        txt.gsub! ':', '-'
        txt.gsub! /(.+?) (.+)/, "\\2) \\1/"
        txt.gsub! /^- /, ''
        return txt.gsub!(/^/, '+ ')
      end

      # I think this is merging together items/wish/slashes
      line = file_and_line.pop if file_and_line.last =~ /^\|/
      file = file_and_line.any? ? file_and_line.join('/') : nil

      # If /file.txt/@git/log/, it's the only one to choose, so short-circuit
      is_file = File.file? dir
      if is_file
        file = relative
      end


      # /rev/, so show files for rev...

      if ! file
        #         command = "git show --pretty=oneline --name-status #{rev}"

        relative_flag = relative ? "--relative=#{relative}" : ''
        command = "git diff --pretty=oneline --name-status #{relative_flag} #{rev}~ #{rev}"

        # Rev passed, so show all diffs
        txt = self.diff_internal command, toplevel

        #         txt.sub! /^.+\n/, ''   # Remove 1st line?
        txt.gsub! /^([A-Z]+)\t/, "\\1) "
        txt.gsub! /^M\) /, ''
        return txt.split("\n").sort.map{|l| "+ #{l}\n"}.join('')
      end

      if ! is_file   # If dir
        file = "#{relative}/#{file}" if relative
      end

      # /rev/file, so diff...

      if line.empty?

        options[:no_slash] = 1

        if prefix == "all" || prefix == 0

          # If enter+all, show just contents, not diff
          # Probably broken - hasn't been tested since Unified refactor

          minus_one = prefix == 0 ? "~" : ""
          txt = Console.run("git show #{rev}#{minus_one}:#{file}", :sync=>true, :dir=>toplevel)
          ENV['no_slash'] = "1"
          return Tree.quote txt
        end

        command = "git show #{@@git_diff_options} --pretty=oneline #{rev} -- #{file}"

        txt = self.diff_internal command, toplevel
        txt.sub!(/.+?@@/m, '@@')
        txt.gsub! /^/, '|'
        ENV['no_slash'] = "1"
        return txt
      end


      # /rev/file/line, so jump to file...

      whole_path = is_file ? dir : "#{toplevel}/#{file}"

      self.jump_to_file_in_tree whole_path
      nil
    end

    def self.show_log
      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to show the log for: "

      Launcher.open("#{dir}/@git/log//")
    end

    def self.jump_to_file_in_tree file

      # TODO: decouple from editor
      #   - probably do cursor stuff conditionally, if $el
      #     - pass in other params so it'll work in web interface

      # TODO: Getting ancestors is probably a better approach

      orig = View.cursor

      Search.backward "^ +\|@@" unless Line.matches(/^ +\|@@/)
      inbetween = View.txt(orig, View.cursor)
      inbetween.gsub!(/^ +\|-.*\n/, '')
      inbetween = inbetween.count("\n")
      line = Line.value[/\+(\d+)/, 1]

      View.cursor = orig

      View.open file
      View.to_line(line.to_i + (inbetween - 1))
    end

    # Called by code tree directly
    def self.status_tree project
      dir = self.extract_dir project
      dir = Bookmarks.expand(dir)
      CodeTree.tree_search_option + self.status_tree_internal(dir)
    end

    def self.status_tree_internal dir

      status = Console.run "git status", :dir => dir, :sync => true

      # Grab out modified:...
      found = status.scan(/^#\s+modified: +(.+)/)
      result = []
      found.each do |m|
        result << "#{dir}#{m[0]}"
      end
      Tree.paths_to_tree(result)
    end

    def self.clean! txt
      txt.gsub!(/^ ?index .+\n/, '')
      txt.gsub!(/^ ?--- .+\n/, '')
      txt.gsub!(/^ ?\+\+\+ .+\n/, '')
    end

    def self.extract_dir project
      project.sub(/.+? - /, '').sub(/\/$/, '')
    end


    def self.git_diff dir, file, line, options={}

      is_unadded = false

      # /, so show diff of files...

      if file.nil?
        txt = Console.run "git status", :dir=>dir, :sync=>true
        hash = self.status_to_hash(self.status_internal(txt))

        untracked = hash[:untracked].map{|i| i[1]}
        untracked.map!{|i| "+ untracked) #{i}\n"}

        option = is_unadded ? "- add\n" : "- commit/\n"
        command = "git diff -b --patience --relative #{self.git_diff_options} #{is_unadded ? '' : ' HEAD'}"

        is_file = File.file? dir

        txt = self.diff_internal command, dir

        if txt =~ /^fatal: ambiguous argument 'HEAD': unknown revision/
          txt = self.status_hash_to_bullets hash, is_unadded
        else
          unless txt.empty?

            self.clean! txt
            txt.gsub!(/^/, '  |')

            if is_file
              return txt.sub(/.+\n/, '')   # First "diff..." line deleted
            end

            txt.gsub!(/^  \| ?diff --git .+ b\//, '- ')
          end
        end

        # Add labels back
        if is_unadded   # If unadded, add labels from added (if they exist there)
          hash[:added].each {|i| txt.sub! /^([+-]) #{i[1]}$/, "\\1 #{i[0]}) #{i[1]}"}
        else   # If added, add your label also unadded (or special)
          unadded = hash[:unadded].map{|i| i[1]}
          hash[:added].each do |i|
            # Only add label if file is also unadded, or if label isn't 'modified'
            next unless unadded.member?(i[1]) or i[0] != 'modified'
            txt.sub! /^([+-]) #{i[1]}$/, "\\1 #{i[0]}) #{i[1]}"
          end
        end

        txt << untracked.join("")

        return "
          | There were no differences. Try modifying a file first.
          | Or create a few sample files:
          @git/setup/make sample files/
          ".unindent if ! txt.any?
        return option + txt + "
          - add/
          - delete/
          - revert/
          - unadd/
          ".unindent
      end


      # /file, so show diff...

      if line.nil?   # If no line passed, re-do diff for 1 file

        txt = is_unadded ?
          self.diff_internal("git diff --patience --relative #{self.git_diff_options} #{file}", dir) :
          #           self.diff_internal("git diff --patience --relative #{self.git_diff_options} HEAD #{file}", dir)
          self.diff_internal("git diff --patience -b --relative #{self.git_diff_options} HEAD #{file}", dir)
        self.clean! txt

        if txt.blank?
          file_in_repository = self.file_in_repository? dir, file

          # If not in repository, just show the contents
          if ! file_in_repository
            return "| untracked file...\n|@@ +1\n" + File.read(Bookmarks.expand("#{dir}/#{file}")).gsub(/^/, '|+').gsub("\c@", '.')
          end
          return "| No diffs"
        end

        txt.gsub!(/^ ?diff .+\n/, '')
        txt.gsub!(/^/, '|')

        self.jump_line_number_maybe txt, options

        return txt
      end

      # /file/line, so navigate to file...

      whole_path = "#{dir}/#{file.sub(/\/$/, '')}"

      # If line passed, jump to it
      self.jump_to_file_in_tree whole_path
      nil
    end

    def self.jump_line_number_maybe txt, options

      line_found = options.delete :line_found
      return if ! line_found

      # Get rid of this?  What did it do?  Possibly an early version of the diff where I made the line numbers parents items?
      last = 0

      target_line, target_boundary = 0, 0
      txt.split("\n").each_with_index do |o, i|
        target_line += 1 if o =~ /^\|[+ ]/
        match = o[/^\|@@ .+\+(\d+)/, 1]
        if target_line >= line_found
          break
        end
        if match
          target_boundary = target_line = match.to_i
          last = i
        end
      end
      last += 3
      last += (target_line - target_boundary)

      options[:line_found] = last
    end


    def self.status_hash_to_bullets hash, is_unadded
      if is_unadded   # If unadded, simply use unadded
        return hash[:unadded].map{|i| "+ #{i[1]}\n"}.join('')
      end

      txt = (hash[:unadded].map{|i| "+ #{i[1]}\n"} +
        hash[:added].map{|i| "+ #{i[1]}\n"}).sort.uniq.join('')
    end

    def self.push_internal dest, dir
      Console.run "git push origin #{dest}", :dir=>dir
      nil
    end

    def self.remove_options siblings
      siblings.select{|o| o !~ /^(commit|delete|revert|add|unadd)\// && o !~ /^\|/ }
    end

    def self.commit_internal message, dir

      siblings = Tree.siblings :include_label=>true
      # Remove labels
      siblings = siblings.map{|i| Line.without_label(:line=>i)}
      # Remove "untracked (ignore)"
      siblings = self.remove_options siblings

      unless siblings.any?   # Error if no siblings
        return "@flash/- Provide some files (on lines next to this menu, with no blank lines, and no 'untracked' label!"
      end

      Console.run "git commit -m \"#{message}\" #{siblings.join("\\\n  ")}", :dir=>dir#, :no_enter=>true
    end

    def self.unadd
      dir = Tree.closest_dir yield[:ancestors]

      siblings = Tree.siblings :include_label=>true
      siblings.map!{|i| Line.without_label(:line=>i)}
      siblings = self.remove_options siblings

      return "- No files to unadd (they should be siblings of unadd)!" if siblings.empty?   # Error if no siblings

      command = "git reset #{siblings.join(' ')}"
      txt = Console.sync command, :dir=>dir
      return Tree.quote txt if txt.any?
      "@flash/- unadded!"
    end

    def self.revert
      dir = Tree.closest_dir yield[:ancestors]

      siblings = Tree.siblings :include_label=>true
      siblings.map!{|i| Line.without_label(:line=>i)}
      siblings = self.remove_options siblings

      return "- No files to revert (they should be siblings of revert)!" if siblings.empty?   # Error if no siblings

      command = "git checkout #{siblings.join(' ')}"
      txt = Console.sync command, :dir=>dir
      return Tree.quote txt if txt.any?
      "@flash/- reverted!"
    end

    def self.delete
      dir = Tree.closest_dir yield[:ancestors]

      siblings = Tree.siblings :include_label=>true
      siblings.map!{|i| Line.without_label(:line=>i)}
      siblings = self.remove_options siblings

      return "- No files to unadd (they should be siblings of delete)!" if siblings.empty?   # Error if no siblings

      command = "rm #{siblings.join(' ')}"
      txt = Console.sync command, :dir=>dir
      return Tree.quote txt if txt.any?
      "@flash/- deleted!"
    end

    def self.git_diff_options
      @@git_diff_options + (Keys.prefix_u ? ' --color-words' : '')
    end

    def self.search_just_push
      match = Search.stop

      # .code_tree_diff was deprecated in Unified refactor - see do+push
      self.code_tree_diff
      View.to_highest

      Search.isearch match
    end

    def self.search_repository
      # .code_tree_diff was deprecated in Unified refactor - see do+push
      self.code_tree_diff
    end

    def self.docs
      "
      > How to use
      | Put the @git menu under a path that is (or you want to make into)
      | a git repo, like so:
      |
      | /tmp/myproject/
      |   @git/
      "
    end

    def self.create
      dir = Tree.closest_dir yield[:ancestors]
      result = Console.sync 'git init', :dir=>dir
      "
      | #{result.strip}
      "
    end

    def self.file_in_repository? dir, file
      txt = Console.sync "git ls-files '#{file}'", :dir=>dir
      txt.any?   # It's in the repository if it didn't return blank
    end

end; end
