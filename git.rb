class Git
  def self.menu_before *args

    trunk = Xiki.trunk

    # If not nested, show docs
    return self.docs if trunk.size < 2

    branch = self.branch_name

    if args[0] != "setup" && branch.nil?
      return "| Not a git repository.  Create a new one here?\n- setup/create/"
    end

    # Handle routing of .push manually, since routing requires linebreaks
    if args[0] == "push"
      return self.push branch, *args[1..-1]
    end

    nil
  end

  def self.menu

    branch = self.branch_name

    "
    - .push/#{branch}/
    - .diff/
    - .log/
    - .setup/
      - .create/
      - .make sample files/
    - .docs/
      > How to use
      | Put the @git menu under a path that has or will have a git repo, like
      | so:
      |
      | - /tmp/my_project/
      |   - @git/
    "
  end

  def self.create

    result = Console.sync 'git init', :dir=>Dir.pwd
    "
    | #{result.strip}
    "
  end

  def self.make_sample_files

    Dir.mkdir "d" rescue nil

    txt = "hello\nhi again\n"
    ["a.txt", "b.txt", "d/aa.txt"].each { |path| File.open(path, "w") { |f| f << txt } }

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

  def self.push default_branch, branch=nil
    # If no branch, use default
    if branch.nil?
      return "- #{default_branch}/"
    end

    Gito.push branch, "project - #{Dir.pwd}"
    ".flash - done!"
  end

  def self.branch_name dir=nil
    dir ||= Dir.pwd
    Console.run("git status", :sync=>true, :dir=>dir)[/# On branch (.+)/, 1]
  end

  def self.diff *args
    if args == [".commit"]
      return View.prompt "Type a commit message."
    end

    path, quote = nil, nil
    if args.any?
      args = args.join "/"
      path, quote = args =~ /\|/ ?
        args.match(/(.+?)\|(.+)/)[1..2] :
        args
    end

    Gito.diff :expand, "project - #{Dir.pwd}", path, quote
    nil
  end

  def self.log *args
    search = ""   # Don't handle this for now
    Gito.log search, "project - #{Dir.pwd}", *args
  end

  def self.add
    Gito.add "project - #{Dir.pwd}"
  end

  def self.commit message=nil
    return View.prompt "Enter a commit message" if message.nil?

    Gito.commit message, "project - #{Dir.pwd}"
  end

  def self.show_log_one_file
    Gito.show_log_one_file
  end

end


# Old git menu
# - Gito.menu/
#   - project - /projects/xiki/xiki_git/
#     + .create/
#     + .diff_unadded/
#     + .diff_unadded :expand/
#     + .diff/
#     + .diff :expand/
#     + .push "master"
#     + .pull
#     + .log ""/
#     + .log :expand/
#     + .log_by_file/
#     + .status/
#     + .status_tree/
#     + .branches/
#     + .stash/
#     - .files/
#     - .format_diff_command "git diff 2b58e1e3b59ff8b5a6c5baf355501c0771b53097 code.rb"/
