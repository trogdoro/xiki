# Note:
#   This file (git.rb) has sort a a weird implementation currently, because
#   repository.rb was recently mergend into git.rb.  Repository.rb used to be
#   separate and behaved as a common interface to git.rb and svn.rb.

class Git
  extend ElMixin

  def self.diff_internal command, dir
    txt = Console.run(command, :sync => true, :dir => dir)

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

  def self.status dir
    # If no file, show status
    txt = Console.run("git status", :sync => true, :dir => dir)
    self.status_internal(txt)
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


  # Reusable - used to be in repository.rb

  # class Repository
  #   extend ElMixin

  CODE_SAMPLES = %q<
    # Show options for the repository
    - Show options: Git.menu
  >

  @@git_diff_options = ' -U2 '
  #@@git_diff_options = ' -U2 -w '   # -w caused a git segfault :/

  def self.svn?
    File.exists?("#{View.dir}.svn")
  end

  def self.tag to, from=nil
    unless self.url
      puts "Error: Git.url isn't set!"
      return
    end
    from ||= "trunk"
    command = "svn -m \"Tag: Create #{from} tag\" cp #{Git.url}/#{from} #{Git.url}/#{to}"
    Console.run(command)
  end

  def self.url= url
    @@url = url
  end

  def self.url
    @@url
  end

  def self.menu project=nil
    # If no project, show all projects
    if project.nil?
      result = []

      result += (Projects.listing.map{|k, v| "#{k} - #{v}/"}.sort)

      # If current dir is in a repos, add it
      current_dir_repos = self.git_path
      result << FileTree.add_slash_maybe("current dir - #{current_dir_repos ? current_dir_repos : View.dir}")

      # If current dir isn't one, try after bar
      if ! current_dir_repos and View.bar?
        after_bar = View.dir_of_after_bar
        current_dir_repos = self.git_path(after_bar)
        result << FileTree.add_slash_maybe(
          "upper - #{current_dir_repos ? current_dir_repos : after_bar}")
      end

      result << "tmp dir - /tmp/t1/"

      return result
    end

    # If project, show options
    puts %Q[
      + .create/
      + .diff_unadded/
      + .diff_unadded :expand/
      + .diff/
      + .diff :expand/
      + .push
      + .pull
      + .log ""/
      + .log :expand/
      + .log_by_file/
      + .status/
      + .status_tree/
      + .branches/
      + .stash/
      - .files/
      - .format_diff_command "git diff 2b58e1e3b59ff8b5a6c5baf355501c0771b53097 code.rb"/
      ].strip.gsub(/^      /, '')
  end

  # Shows revs for one file
  def self.log_by_file search, limit, project, file=nil, rev=nil, line=nil
    dir = self.extract_dir project
    if file.nil?   # If no file, tell them they have to paste it
      return "- Replace this line with a path - I'm normally called via Keys.show_log_one_file"
    end

    if rev.nil?   # If no rev, list all revs
      search = "-S'#{search}'" unless search.empty?
      txt = Console.run "git log -#{limit} --pretty=oneline #{search} #{file}", :sync=>true, :dir=>dir
      txt.gsub! ':', '-'
      txt.gsub! /(.+?) (.+)/, "\\2: \\1"
      txt.gsub! /^- /, ''
      return txt.gsub!(/^/, '+ ')
      #return "- TODO: show all revs"
    end

    if line.nil?   # If no diff, show diff
      # File passed, show diff
      txt = Git.diff_internal "git show #{@@git_diff_options} --pretty=oneline #{rev} #{file}", dir
      txt.sub!(/.+?@@/m, '@@')
      return txt.gsub /^/, '|'
    end

    # Line passed, so jump to it
    project.sub! /^project - /, ''
    self.jump_to_file_in_tree project
    nil
  end

  def self.log search, project, rev=nil, file=nil, line=nil
    dir = self.extract_dir project

    if search == :expand
      Ol << 'implement: expand all files!'
      return "- implement!"
    end

    if rev.nil?   # If no rev, list all revs
      search = "-S'#{search}'" unless search.empty?
      txt = Console.run "git log -1000 --pretty=oneline #{search}", :sync=>true, :dir=>dir
      txt.gsub! ':', '-'
      txt.gsub! /(.+?) (.+)/, "\\2: \\1"
      txt.gsub! /^- /, ''
      return txt.gsub!(/^/, '+ ')
    end

    if file.nil?   # If no file, show files for rev
      # Rev passed, so show all diffs
      txt = Git.diff_internal "git show --pretty=oneline --name-status #{rev}", dir
      txt.sub! /^.+\n/, ''
      txt.gsub! /^([A-Z])\t/, "\\1: "
      txt.gsub! /^M: /, ''
      return txt.split("\n").sort.map{|l| "+ #{l}\n"}.join('')
    end

    if line.nil?   # If no line but file passed, show diff
      txt = Git.diff_internal "git show #{@@git_diff_options} --pretty=oneline #{rev} #{file}", dir
      txt.sub!(/.+?@@/m, '@@')
      txt.gsub! /^/, '|'
      puts txt
      return
    end

    # Line passed, so jump to file
    project.sub! /^project - /, ''
    self.jump_to_file_in_tree project
    nil
  end

  def self.diff_one_file
    self.git_diff_one_file
  end

  def self.git_diff_one_file
    repos = self.git_path   # Get root of repos
    relative = View.file.sub(/^#{repos}/, '')   # Split off root from relative path
    relative.sub! /^\//, ''

    if Keys.prefix_u :clear=>true

      orig_path = "/tmp/#{View.file_name}__orig"

      # Get relative path
      txt = Console.run "git show HEAD:#{relative}", :sync=>true, :dir=>repos
      File.open(orig_path, "w") { |f| f << txt }

      $el.ediff_files orig_path, View.file

      return
    end

    # Insert codetree
    CodeTree.display_menu(
      "- Git.menu/\n  - project - #{repos}\n    - .diff/\n      - #{relative}",
      :no_search=>true
      )
  end

  def self.show_log
    dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to show the log for: "
    CodeTree.display_menu("- Git.menu/\n  - project - #{dir}\n    - .log ''/")
  end

  def self.show_log_one_file
    repos = self.git_path   # Get root of repos
    relative = View.file.sub(/^#{repos}/, '')   # Split off root from relative path
    relative.sub! /^\//, ''   # Insert codetree

    CodeTree.display_menu(
      "- Git.menu/\n  - project - #{repos}\n    - .log_by_file \"\", 10/\n      - #{relative}"
      )
  end

  def self.jump_to_file_in_tree dir, options={}
    orig = View.cursor

    Search.backward "^ +\|@@" unless Line.matches(/^ +\|@@/)
    inbetween = View.txt(orig, View.cursor)
    inbetween.gsub!(/^ +\|-.*\n/, '')
    inbetween = inbetween.count("\n")
    line = Line.value[/\+(\d+)/, 1]

    Search.backward "^ +[+-] "
    file = options[:file] || Line.without_label

    goto_char orig

    View.open("#{dir}/#{file}")
    View.to_line(line.to_i + (inbetween - 1))
    Color.colorize :l
  end

  def self.styles_define
    Styles.define :face_difflog_path_outline_rest,
      :fg => "000077",
      :bg => "7070bb",
      :font => "arial"

    # - foo (r): <here>
    Styles.define :diff_subhead,
      :bg => "333366", :fg => "88b", :size => "-3"

  end

  def self.styles
    cm_universal_diff_format
    difflog_highlight
    Styles.apply "^diff.+\n", :diff_subhead
    Styles.apply "^Index:? .+\n", :diff_subhead
    Styles.apply "^===+\n", :diff_subhead
    Styles.apply "^--- .+\n", :diff_subhead
    Styles.apply "^\\+\\+\\+ .+\n", :notes_h1
    Styles.apply "^\\\\.+", :cm_lighter_bold
    #Styles.apply "^\\*.+\n", :treels_f_blank
    Styles.apply "^@@.+\n", :diff_subhead

  end

  def self.git? dir
    git_status = Git.diff_internal "git status", dir
    git_status !~ /^fatal: Not a git repository/
  end

  def self.git_path dir=nil
    dir ||= View.dir
    return nil unless self.git?(dir)
    dir = Console.run("git rev-parse --git-dir", :sync=>true,
      :dir=>dir
      ).sub(".git\n", '')
    dir = View.dir if dir == ""   # Empty actually means it found it
    dir
  end

  def self.determine_dir dir
    self.git_path(dir)# || Bookmarks['$tr']
  end

  def self.open_list_repository
    # Figure out which dir
    dir = Keys.prefix_u? ? Bookmarks['$tr'] : self.determine_dir(View.dir)

    View.to_buffer "*git status"
    View.clear
    View.dir = dir
    FileTree.apply_styles
    use_local_map elvar.notes_mode_map

    tree = self.status_tree_internal dir
    View.insert "- Git.status_tree/\n#{tree.gsub(/^/, '  ')}"

    View.to_top
    Move.to_junior
    FileTree.search :recursive => true

  end

  # Called by code tree directly
  def self.status_tree project
    dir = self.extract_dir project
    dir = Bookmarks.expand(dir)
    puts CodeTree.tree_search_option + self.status_tree_internal(dir)
  end

  def self.status_tree_internal dir

    if self.svn?
      status = Console.run "svn st", :dir => dir, :sync => true
      # Remove question files
      status = status.split("\n")#.select{|l| l !~ /^\?/}
      status = status.each{|l| l.sub!(/^. +/, dir)}
      FileTree.paths_to_tree(status)
    else   # git
      status = Console.run "git status", :dir => dir, :sync => true

      # Grab out modified:...
      found = status.scan(/^#\s+modified: +(.+)/)
      result = []
      found.each do |m|
        result << "#{dir}#{m[0]}"
      end
      FileTree.paths_to_tree(result)
    end
  end

  def self.clean! txt
    txt.gsub!(/^ ?index .+\n/, '')
    txt.gsub!(/^ ?--- .+\n/, '')
    txt.gsub!(/^ ?\+\+\+ .+\n/, '')
  end

  def self.extract_dir project
    project.sub(/.+? - /, '').sub(/\/$/, '')
  end

  def self.status project, file=nil
    dir = self.extract_dir project
    #dir ||= self.determine_dir

    if file.nil?
      return Git.status dir
    end

    View.open("#{dir}/#{file}")   # If file, open it

  end

  def self.diff_unadded *args
    expand = args.shift if args.first.is_a? Symbol   # Pull out :expand if 1st arg
    project, file, line = args
    dir = self.extract_dir project

    self.git_diff_unadded(expand, dir, file, line)
  end

  def self.git_diff_unadded expand, dir, file, line
    self.git_diff_or_diff_unadded true, expand, dir, file, line
  end

  def self.diff *args
    #   def self.do_compare_one *args
    # Parse args
    expand = args.shift if args.first.is_a? Symbol   # Pull out :expand if 2nd arg
    project, file, line = args
    dir = self.extract_dir project

    if self.git?(dir)
      self.git_diff expand, dir, file, line
    else
      self.svn_diff expand, dir, file, line
    end
  end

  def self.git_diff expand, dir, file, line

    self.git_diff_or_diff_unadded false, expand, dir, file, line

  end

  def self.git_diff_or_diff_unadded is_unadded, expand, dir, file, line
    if file.nil?   # If launching .diff/.diff_unadded directly (not a file)

      txt = Console.run "git status", :dir=>dir, :sync=>true

      hash = Git.status_to_hash(Git.status_internal(txt))

      untracked = hash[:untracked].map{|i| i[1]}
      untracked.map!{|i| "+ untracked#{is_unadded ? '' : ' (ignore)'}: #{i}\n"}

      option = is_unadded ? "- action: .add\n" : "- action: .commit \"message\"\n"

      if expand   # If showing diffs right away
        txt = Git.diff_internal "git diff --patience --relative #{self.git_diff_options}#{is_unadded ? '' : ' HEAD'}", dir

        if txt =~ /^fatal: ambiguous argument 'HEAD': unknown revision/
          txt = "- Warning: Couldn't diff because no revisions exist yet in repository\n" +
            "  - Try using: .diff/ (without :expand)\n"
        else
          unless txt.empty?
            self.clean! txt
            txt.gsub!(/^/, '  |')
            txt.gsub!(/^  \| ?diff --git .+ b\//, '- ')
          end
        end
      else   # If just showing list of files
        if is_unadded   # If unadded, simply use unadded
          txt = hash[:unadded].map{|i| "+ #{i[1]}\n"}.join('')
        else   # If added, use added + unadded - dups
          txt = (hash[:unadded].map{|i| "+ #{i[1]}\n"} +
            hash[:added].map{|i| "+ #{i[1]}\n"}).sort.uniq.join('')
        end
      end

      # Add labels back
      if is_unadded   # If unadded, add labels from added (if they exist there)
        hash[:added].each {|i| txt.sub! /^([+-]) #{i[1]}$/, "\\1 #{i[0]}: #{i[1]}"}
      else   # If added, add your label also unadded (or special)
        unadded = hash[:unadded].map{|i| i[1]}
        hash[:added].each do |i|
          # Only add label if file is also unadded, or if label isn't 'modified'
          next unless unadded.member?(i[1]) or i[0] != 'modified'
          txt.sub! /^([+-]) #{i[1]}$/, "\\1 #{i[0]}: #{i[1]}"
        end
      end
      txt << untracked.join("")
      txt = "- Warning: nothing to show" if ! txt.any?

      return CodeTree.no_search_option + option + txt + "- .add\n- .delete\n- revert: .checkout\n"
    end

    if line.nil?   # If no line passed, re-do diff for 1 file
      # If untracked, show whole file
      if Line.label =~ /^untracked/
        return "|@@ +1\n" + IO.read(Bookmarks.expand("#{dir}/#{file}")).gsub(/^/, '|+').gsub("\c@", '.')
      end

      txt = is_unadded ?
        Git.diff_internal("git diff --patience --relative #{self.git_diff_options} #{file}", dir) :
        Git.diff_internal("git diff --patience --relative #{self.git_diff_options} HEAD #{file}", dir)
      self.clean! txt
      txt.gsub!(/^ ?diff .+\n/, '')
      txt.gsub!(/^/, '|')
      return puts(txt)
    end

    # If line passed, jump to it
    self.jump_to_file_in_tree dir
    nil
  end

  def self.format_diff_command command, project, line=nil
    dir = self.extract_dir project

    # If no line, just do diff
    if line.nil?
      txt = Console.run command, :dir=>dir, :sync=>true
      return txt.gsub!(/^/, '  |')
    end

    self.jump_to_file_in_tree dir, :file=>command[/.+ (.+)/, 1]
    nil
  end

  def self.push project
    dir = self.extract_dir project
    Console.run "git push origin master", :dir=>dir
    nil
  end

  def self.pull project
    dir = self.extract_dir project
    Console.run "git pull origin master", :dir=>dir
    nil
  end

  def self.code_tree_diff options={}
    dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to git diff in: "

    prefix = Keys.prefix
    expand = prefix == :uu ? "" : ", :expand"
    menu = "- Git.menu/\n  - project - #{dir}\n    - .diff#{expand}/"

    if options[:enter]
      View.insert(menu)
      LineLauncher.launch
    else
      View.bar if prefix == 9
      CodeTree.display_menu(menu)
    end
  end

  def self.code_tree_diff_unadded options={}
    dir = Keys.bookmark_as_path
    menu = "- Git.menu/\n  - project - #{dir}\n    - .diff_unadded :expand/"
    if options[:enter]
      View.insert(menu)
      LineLauncher.launch
    else
      CodeTree.display_menu(menu)
    end
  end

  def self.svn_diff_unadded dir, file
    children = CodeTree.children || []

    if file   # If file passed, just open it
      View.open("#{dir}/#{file}")
      return
    end

    if children.nonempty?   # If children, add them
      children = children.map{|c| c.sub(/^ +[+-] /, '')}.join(" ")
      txt = Console.run("svn add #{children}", :dir => dir)

      return
    end

    # If no children, show ones to be added
    txt = Console.run("svn status", :dir => dir, :sync => true)
    return txt.scan(/^\? +(.+)/)

  end

  def self.svn_diff expand, dir, file, line, children

    if file.nil?   # If launching .commit directly (no file)

      if expand
        txt = Console.run("svn diff #{file}", :sync => true, :dir => dir)

        txt.gsub!(/^===+\n/, '')
        txt.gsub!(/^--- .+\n/, '')
        txt.gsub!(/^\+\+\+ .+\n/, '')
        txt.gsub!(/^/, '  |')
        txt.gsub!(/^  \|Index: /, '- ')
        return txt

      else
        txt = Console.run "svn status", :dir=>dir, :sync=>true

        modified = txt.scan(/^M +(.+)/).map{|i| i.first}
        new_files = txt.scan(/^A +(.+)/).map{|i| "new: " + i.first}

        return new_files + modified
      end
    end

    if line.nil?   # If no line passed, re-do diff for 1 file
      txt = Console.run("svn diff #{file}", :sync => true, :dir => dir)

      txt.gsub!(/^Index: .+\n/, '')
      txt.gsub!(/^===+\n/, '')
      txt.gsub!(/^--- .+\n/, '')
      txt.gsub!(/^\+\+\+ .+\n/, '')
      txt.gsub!(/^/, '|')
      return txt
    end

    self.jump_to_file_in_tree dir   # If line passed, jump to it

    nil   # Be sure to have no return value

  end

  def self.add project
    dir = self.extract_dir project

    siblings = CodeTree.siblings
    # Error if no siblings
    unless siblings.any?
      return "- No files to add (they should be siblings of .add)!"
    end

    if self.git?(dir)
      Console.run("git add #{siblings.join(' ')}", :dir=>dir)
    else
      Console.run("svn add #{siblings.join(' ')}", :dir=>dir)
    end
  end

  def self.commit message, project
    dir = self.extract_dir project

    siblings = CodeTree.siblings :include_label=>true

    # Remove "untracked (ignore)"
    siblings = siblings.select{|i| i !~ /^. untracked \(ignore\)/}.map{|i| Line.without_label(:line=>i)}

    if message == 'message'   # Error if left default commit message
      return "- Error: You must change 'message' to be your commit message." if message == "message"
    end

    unless siblings.any?   # Error if no siblings
      return "- Error: No files to commit\n" +
             "- They should be siblings of .commit, and already be added!"
    end

    if self.git?(dir)
      Console.run "git commit -m \"#{message}\" #{siblings.join(' ')}", :dir=>dir#, :no_enter=>true
    else
      Console.run "svn ci -m \"#{message}\" #{siblings.join(' ')}", :dir=>dir
    end
  end

  def self.checkout project#, file=nil
    dir = self.extract_dir project

    siblings = CodeTree.siblings :include_label=>true
    siblings.map!{|i| Line.without_label(:line=>i)}

    unless siblings.any?   # Error if no siblings
      return "- Error: No files to checkout\n" +
             "- They should be siblings of .checkout!"
    end

    Console.run "git checkout #{siblings.join(' ')}", :dir=>dir #, :no_enter=>true
  end

  def self.delete project
    dir = self.extract_dir project

    siblings = CodeTree.siblings :include_label=>true
    siblings.map!{|i| Line.without_label(:line=>i)}

    unless siblings.any?   # Error if no siblings
      return "- Error: No files to delete\n" +
             "- They should be siblings of .delete!"
    end

    Console.run "rm #{siblings.join(' ')}", :dir=>dir #, :no_enter=>true
  end

  def self.create project
    "
    - #{self.extract_dir(project)}/
      - create: ! git init
      - make test files: ! echo 'a\\na\\na' > a.txt; echo 'b\\nb\\nb' > b.txt
      - add files: ! git add .
      - commit: ! git commit -m \"First commit.\"
    "
  end

  #   def self.initialize project
  #     dir = self.extract_dir project
  #     # Create dir if not there
  #     Dir.mkdir(dir) if not File.directory?(dir)
  #     Console.run("git init", :dir => dir)
  #     nil
  #   end

  def self.files project
    "- #{self.extract_dir(project)}/"
  end

  def self.branches project
    "
    - #{self.extract_dir(project)}/
      - list branches:
      ! git branch
      ! git branch -r
      - create branch:
      ! git branch foo
        - switch to:
        ! git checkout foo
        - delete:
        ! git branch -d foo
      - switch to main branch:
      ! git checkout master
      - delete remote branch:
      ! git push origin :my_branch
    "
  end

  def self.stash project
    "
    - #{self.extract_dir(project)}/
      - Temporarily put away uncommitted changes:
      ! git stash
      - Restore uncommitted changes:
      ! git stash apply
      ! git stash pop
      - list: ! git stash list
      - files: ! git stash show
      - diff: ! git stash show --patience
    "
  end

  def self.git_diff_options
    @@git_diff_options + (Keys.prefix_u ? ' --color-words ' : '')

    # Doesn't work:
    #  + (Keys.prefix_uu ? ' --name-only ' : '')
  end

  def self.search_just_push
    match = Search.stop

    Git.code_tree_diff
    View.to_highest
    #     Search.isearch nil

    # Ol.line
    #     DiffLog.open
    #     View.to_bottom

    Search.isearch match #, :reverse=>true

  end

end

Git.styles_define
