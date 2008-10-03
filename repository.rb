class Repository
  extend ElMixin

  CODE_SAMPLES = %q<
    # Show options for the repository
    - Show options: Repository.menu
  >

  def self.svn?
    File.exists?("#{View.dir}.svn")
  end

  def self.tag to, from=nil
    unless Repository.url
      puts "Error: Repository.url isn't set!"
      return
    end
    from ||= "trunk"
    command = "svn -m \"Tag: Create #{from} tag\" cp #{Repository.url}/#{from} #{Repository.url}/#{to}"
    Shell.run(command)
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

      # Add current dir, if repos
      current_dir_repos = self.git_path
      result << "+ current dir - #{current_dir_repos}/" if current_dir_repos
      result += (Projects.listing.map{|k, v| "+ #{k} - #{v}/"}.sort)
      return result
    end

    # If project, show options
    puts %Q[
      + .diff_unadded/
      + .diff_unadded :expand/
      + .diff/
      + .diff :expand/
      + .push
      + .log ""/
      + .status/
      + .status_tree/
      - .initialize
      - .files/
      ].strip.gsub(/^      /, '')
  end

  def self.log search, project, rev=nil, file=nil
    dir = self.extract_dir project

    if rev.nil?   # If no rev, list all revs
      search = "-S'#{search}'" unless search.empty?

      txt = Shell.run "git log --pretty=oneline #{search}", :sync=>true, :dir=>dir
      txt.gsub! ':', '-'
      txt.gsub! /(.+?) (.+)/, "\\2: \\1"
      txt.gsub! /^- /, ''
      return txt.gsub! /^/, '+ '
    end

    if file.nil?   # If no file, show files for rev
      # Rev passed, so show all diffs
      txt = Shell.run "git show --pretty=oneline --name-status -U2 #{rev}", :sync=>true, :dir=>dir
      txt.sub! /^.+\n/, ''
      txt.gsub! /^([A-Z])\t/, "\\1: "
      txt.gsub! /^M: /, ''
      return txt.split("\n").sort.map{|l| "+ #{l}"}
    end

    # File passed, show diff
    txt = Shell.run "git show --pretty=oneline -U2 #{rev} #{file}", :sync=>true, :dir=>dir
    txt.sub!(/.+?@@.+?\n/m, '')
    txt.gsub! /^-/, '~'
    txt.gsub! /^/, '|'
    puts txt
    return
  end

  def self.diff_one_file
    #     if self.svn?
    #       cm_subversion_command("echo; svn diff -x -w #{file_name_nondirectory(buffer_file_name)}")
    #     else
    self.git_diff_one_file
    #     end
  end

  def self.git_diff_one_file
    repos = self.git_path   # Get root of repos
    relative = View.file.sub(/^#{repos}/, '')   # Split off root from relative path

    relative.sub! /^\//, ''   # Insert codetree

    CodeTree.display_menu(
      "Repository.menu/\n  - project - #{repos}\n    - .diff/\n      - #{relative}"
      )

  end

  def self.jump_to_file_in_tree dir
    orig = View.cursor
    Search.backward "^ +\|@@" unless Line.matches(/^ +\|@@/)

    inbetween = View.txt(orig, View.cursor)
    inbetween.gsub!(/^ +\|~.*\n/, '')
    inbetween = inbetween.count("\n")
    line = Line.value[/\+(\d+)/, 1]
    Search.backward "^ +- "
    file = Line.without_label
    goto_char orig

    View.open("#{dir}/#{file}")
    View.to_line(line.to_i + (inbetween - 1))
    Color.colorize :o
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
    Styles.apply "^\\*.+\n", :treels_f_blank
    Styles.apply "^@@.+\n", :diff_subhead

  end

  def self.git? dir
    git_status = Shell.run "git status", :sync => true, :dir => dir
    git_status !~ /^fatal: Not a git repository/
  end

  def self.git_path dir=nil
    dir ||= View.dir
    return nil unless self.git?(dir)
    dir = Shell.run("git rev-parse --git-dir", :sync=>true,
      :dir=>dir
      ).sub(".git\n", '')
    dir = View.dir if dir == ""
    dir
  end

  def self.determine_dir dir
    self.git_path(dir) || Bookmarks['$tr']
  end

  def self.open_list_repository
    # Figure out which dir
    dir = Keys.prefix_u? ? Bookmarks['$tr'] : self.determine_dir(View.dir)

    View.to_buffer "*repository status"
    View.clear
    View.dir = dir
    TreeLs.apply_styles
    use_local_map elvar.notes_mode_map

    tree = self.status_tree_internal dir
    View.insert "- Repository.status_tree/\n#{tree.gsub(/^/, '  ')}"

    View.to_top
    Move.to_junior
    TreeLs.search :recursive => true

  end

  # Called by code tree directly
  def self.status_tree project
    dir = self.extract_dir project
    dir = Bookmarks.expand(dir)
    puts CodeTree.tree_search_option + self.status_tree_internal(dir)
  end

  def self.status_tree_internal dir

    if self.svn?
      status = Shell.run "svn st", :dir => dir, :sync => true
      # Remove question files
      status = status.split("\n")#.select{|l| l !~ /^\?/}
      status = status.each{|l| l.sub!(/^. +/, dir)}
      TreeLs.paths_to_tree(status)
    else   # git
      status = Shell.run "git status", :dir => dir, :sync => true

      # Grab out modified:...
      found = status.scan(/^#\s+modified: +(.+)/)
      result = []
      found.each do |m|
        result << "#{dir}#{m[0]}"
      end
      TreeLs.paths_to_tree(result)
    end
  end

  def self.clean! txt
    txt.gsub!(/^index .+\n/, '')
    txt.gsub!(/^--- .+\n/, '')
    txt.gsub!(/^\+\+\+ .+\n/, '')
  end

  def self.extract_dir project
    project.sub(/.+? - /, '').sub(/\/$/, '')
  end

  def self.status project, file=nil
    dir = self.extract_dir project
    #dir ||= self.determine_dir

    # If no file, show status
    if file.nil?
      txt = Shell.run("git status", :sync => true, :dir => dir)
      txt.gsub!(/^#\t/, '- ')
      txt.gsub!(/^#\n/, '')
      txt.gsub!(/^#/, '|')
      return puts(txt)
    end

    View.open("#{dir}/#{file}")   # If file, open it

  end

  def self.diff_unadded *args
    expand = args.shift if args.first.is_a? Symbol   # Pull out :expand if 1st arg
    project, file, line = args
    dir = self.extract_dir project

    if self.git?(dir)
      self.git_diff_unadded(expand, dir, file, line)
    else
      self.svn_diff_unadded(dir, file)
    end
  end


  def self.git_diff_unadded expand, dir, file, line
    self.git_diff_or_diff_unadded true, expand, dir, file, line
  end

  def self.diff *args
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

      txt = Shell.run "git status", :dir=>dir, :sync=>true
      new_files = txt.scan(/\tnew file: +(.+)$/).map{|a| a.first}
      untracked = txt.scan(/\t([^:\n]+$)/).map{|a| a.first}
      modified = txt.scan(/\tmodified: +(.+$)/).map{|a| a.first}.uniq

      if is_unadded   # If diffing with repos add 'new' label to new files
        untracked.map!{|i| "- untracked: #{i}"}
      else
        untracked.map!{|i| "- untracked (ignore): #{i}"}
      end

      option = is_unadded ? "- action: .add\n" : "- action: .commit \"message\"\n"

      if expand
        txt = is_unadded ?
          Shell.run('git diff -U2 -w', :sync => true, :dir => dir) :
          Shell.run('git diff -U2 -w HEAD', :sync => true, :dir => dir)

        if txt =~ /^fatal: ambiguous argument 'HEAD': unknown revision/
          txt = "- Warning: Couldn't diff because no revisions exist yet in repository\n" +
            "  - Try using: - .diff/ (without :expand)"
        else
          unless txt.empty?
            self.clean! txt
            txt.gsub!(/^-/, '~')
            txt.gsub!(/^/, '  |')
            txt.gsub!(/^  \|diff --git .+ b\//, '- ')
          end
        end
        untracked.map!{|i| "#{i}\n"}
        txt = txt + untracked.join('')
      else
        modified -= new_files if ! is_unadded   # If diffing with repos remove dups
        txt = nil
        if is_unadded
          txt = modified.map{|i| "+ #{i}"}
        else
          txt = (modified + new_files).sort.map{|i| "+ #{i}"}
        end
        txt = (txt + untracked).join("\n")
      end

      if ! is_unadded   # If diffing with repos add 'new' label to new files
        new_files.each do |i|
          txt.sub! /^([+-]) #{i}$/, "\\1 new: #{i}"
        end
      end
      if ! txt.any?
        txt = "- Warning: nothing to show"
      end
      return option + txt
    end

    if line.nil?   # If no line passed, re-do diff for 1 file
      # If untracked, show whole file
      if Line.label =~ /^untracked/
        return "|@@ +1\n" + IO.read(Bookmarks.expand("#{dir}/#{file}")).gsub(/^/, '|+')
      end

      txt = is_unadded ?
        Shell.run("git diff -U2 -w #{file}", :sync => true, :dir => dir) :
        Shell.run("git diff -U2 -w HEAD #{file}", :sync => true, :dir => dir)
      self.clean! txt
      txt.gsub!(/^diff .+\n/, '')
      txt.gsub!(/^-/, '~')
      txt.gsub!(/^/, '|')
      return puts(txt)
    end

    # If line passed, jump to it
    self.jump_to_file_in_tree dir
    nil
  end

  def self.push project
    dir = self.extract_dir project
    Shell.run "git push origin master", :dir=>dir
    nil
  end

  def self.code_tree_diff options={}
    bookmark = Keys.input(:timed => true, :prompt => "Repository diff in which dir? (enter bookmark): ")
    menu = "- Repository.menu/\n  - project - $#{bookmark}/\n    - .diff, :expand/"
    if options[:enter]
      View.insert(menu)
      LineLauncher.launch
    else
      CodeTree.display_menu(menu)
    end
  end

  def self.code_tree_diff_unadded options={}
    bookmark = Keys.input(:timed => true, :prompt => "Repository diff in which dir? (enter bookmark): ")
    menu = "- Repository.menu/\n  - project - $#{bookmark}/\n    - .diff_unadded :expand/"
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
      txt = Shell.run("svn add #{children}", :dir => dir)

      return
    end

    # If no children, show ones to be added
    txt = Shell.run("svn status", :dir => dir, :sync => true)
    return txt.scan(/^\? +(.+)/)

  end

  def self.svn_diff expand, dir, file, line, children

    if file.nil?   # If launching .commit directly (no file)

      if expand
        txt = Shell.run("svn diff #{file}", :sync => true, :dir => dir)

        txt.gsub!(/^===+\n/, '')
        txt.gsub!(/^--- .+\n/, '')
        txt.gsub!(/^\+\+\+ .+\n/, '')
        txt.gsub!(/^-/, '~')
        txt.gsub!(/^/, '  |')
        txt.gsub!(/^  \|Index: /, '- ')
        return txt

      else
        txt = Shell.run "svn status", :dir=>dir, :sync=>true

        modified = txt.scan(/^M +(.+)/).map{|i| i.first}
        new_files = txt.scan(/^A +(.+)/).map{|i| "new: " + i.first}

        return new_files + modified
      end
    end

    if line.nil?   # If no line passed, re-do diff for 1 file
      txt = Shell.run("svn diff #{file}", :sync => true, :dir => dir)

      txt.gsub!(/^Index: .+\n/, '')
      txt.gsub!(/^===+\n/, '')
      txt.gsub!(/^--- .+\n/, '')
      txt.gsub!(/^\+\+\+ .+\n/, '')
      txt.gsub!(/^-/, '~')
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
      Shell.run("git add #{siblings.join(' ')}", :dir=>dir)
    else
      Shell.run("svn add #{siblings.join(' ')}", :dir=>dir)
    end
  end

  def self.commit message, project
    dir = self.extract_dir project

    siblings = CodeTree.siblings :include_label=>true

    left1, right1, left2, right2 = CodeTree.sibling_bounds
    #Effects.blink :left=>left1, :right=>right1
    Effects.blink :left=>left2, :right=>right2

    # Remove untracked
    siblings = siblings.select{|i| i !~ /^. untracked/}.map{|i| Line.without_label(:line=>i)}

    if message == 'message'   # Error if no siblings
      return "- Error: You must change 'message' to be your commit message." if message == "message"
    end
    unless siblings.any?
      return "- Error: No files to commit\n" +
             "  - They should be siblings of .commit, and already be added."
    end

    if self.git?(dir)
      Shell.run("git commit -m \"#{message}\" #{siblings.join(' ')}", :dir=>dir)
    else
      Shell.run("svn ci -m \"#{message}\" #{siblings.join(' ')}", :dir=>dir)
    end
  end

  def self.initialize project
    dir = self.extract_dir project
    # Create dir if not there
    Dir.mkdir(dir) if not File.directory?(dir)
    Shell.run("git init", :dir => dir)
    nil
  end

  def self.files project
    "- #{self.extract_dir(project)}/"
  end

end
Repository.styles_define
