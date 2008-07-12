class Repository
  extend ElMixin

  CODE_SAMPLES = %q<
    # Show options for the repository
    - Show options: Repository.menu
  >

  def self.svn_command command, dir=nil
    dir ||= View.dir
    #dir ||= "tr"
    switch_to_buffer generate_new_buffer("*repository command")
    elvar.default_directory = Bookmarks.expand(dir)
    shell current_buffer
    #cm_universal_diff_format
    self.styles
    self.local_keys
    #difflog_highlight
    insert command
    Shell.enter
  end

  def self.svn?
    File.exists?("#{View.dir}.svn")
  end

  def self.diff_dir

    # if .svn dir found
    if self.svn?
      self.svn_command("echo; svn status; svn diff -x -w")
    else
      options = Keys.prefix_u ? "$tr" : View.dir
      self.svn_command("git status; git diff -w . ", options)
    end

#     if Line.matches(/^r(\d+)/)
#       r = Line.value[/\d+/]
#       self.svn_command("echo; svn status; svn diff -x -w -r#{r.to_i - 1}:#{r}")
#       #cm_subversion_command "echo; svn status; svn diff -x -w -r#{r.to_i - 1}:#{r}"
#       return
#     end

  end

  def self.diff right=nil, left=nil, bookmark=nil
    right ||= Keys.input.to_i
    left ||= right - 1
    self.svn_command("echo; svn diff -x -w -r#{left}:#{right}", "$#{bookmark}")
  end

  # Requires that this line be set somewhere:
  #   Repository.url = "http://svn.foo.com/svn/repos/foo"
  #     - Note that the above path should be to the dir containing
  #       trunk/, tags/, and branches/
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

  def self.menu options={}
    puts %Q[
      - .log/
      - Set repository: Repository.url = "http://svn.foo.com/svn/repos/foo"
      - Diff with previous version: Repository.diff(10, nil, "bookmark")
      - Create tag: Repository.tag("tags/v1_0")
      ].strip.gsub(/^      /, '')
  end

  def self.log
    orig = View.current
    View.to_after_bar
    dir = View.dir
    View.to_window orig
    log = Shell.run('git log', :sync => true, :dir => dir)
    # Replace commit... into separators
    a = log.split(/\n?^commit .+\n.+\n.+\n\n/)
    a.each {|l| l.gsub!(/^    /, '- ')}
    a.each {|l| l.gsub!(/^- - /, '- ')}
    a.each {|l| l.gsub!(/\n/, "  ")}
    puts a.join("\n")
  end

  def self.diff_one_file
    if Keys.prefix_u  # If C-u
      cm_compare_one_svn_file
      return
    end

    if self.svn?
      cm_subversion_command("echo; svn diff -x -w #{file_name_nondirectory(buffer_file_name)}")
    else
      Shell.run("git diff '#{View.file}'")
      #cm_subversion_command("echo; svn diff -x -w #{file_name_nondirectory(buffer_file_name)}")
      self.styles
      self.local_keys
    end
  end

  def self.local_keys
#    elvar.repository_mode_map = make_sparse_keymap
      # Inherit notes_mode_map!
    Keys.CN(:cm_diff_map) do
      Line.to_right
      re_search_forward "^index:? "
      Line.to_left
      recenter 0
    end
    Keys.CP(:cm_diff_map) do
      Line.to_left
      re_search_backward "^index:? "
      recenter 0
    end

    define_key :cm_diff_map, kbd("M-m") do   # Recenter
      orig = View.cursor
      Search.backward "^@@"
      inbetween = View.txt orig, View.cursor
      inbetween.gsub!(/^-.+\n/, '')
      inbetween = inbetween.split(/\n/).size
      line = Line.value[/\+(\d+)/, 1]
      Search.backward "^\\+\\+\\+"
      file = Line.value[/^\+\+\+ (.+)/, 1]
      file.sub!(/\t.+/, '')  # svn
      file.sub!(/^[ab]\//, '')  # git
      goto_char orig
      View.open(file)
      View.to_line(line.to_i + (inbetween - 1))
      Color.colorize :o
    end

  end

  def self.styles_define
    Styles.define :face_difflog_path_outline_rest,
      :fg => "000077",
      :bg => "7070bb",
      :font => "arial"

    # - foo (r): <here>
    Styles.define :diff_subhead,
      :bg => "333366", :fg => "88b", :size => "-3"

    #:box => nil
    #:height => "
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
#       (0 'cm-hbar-front))))
# ;      (0 'face-difflog-path-outline-rest))))

  end

  def self.status_tree

    dir = Keys.prefix_u ? Bookmarks['$tr'] : View.dir

    View.to_buffer "*repository status"
    View.clear
    View.dir = dir
    TreeLs.apply_styles
    use_local_map elvar.notes_mode_map

    if self.svn?
      status = Shell.run "svn st", :dir => dir, :sync => true
      # Remove question files
      status = status.split("\n")#.select{|l| l !~ /^\?/}
      status = status.each{|l| l.sub!(/^. +/, dir)}
      View.insert TreeLs.paths_to_tree(status)
    else   # git
      status = Shell.run "git status", :dir => dir, :sync => true

      # Grab out modified:...
      found = status.scan(/^#\s+modified: +(.+)/)
      result = []
      found.each do |m|
        result << "#{dir}#{m[0]}"
      end
      tree = TreeLs.paths_to_tree(result)
      View.insert tree
    end
    View.to_top
    Move.to_junior
    TreeLs.search :recursive => true

  end

end
Repository.styles_define
