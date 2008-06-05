class Repository
  extend ElMixin

  CODE_SAMPLES = %q<
    # Show options for the repository
    - Show options: Repository.menu
  >

  def self.svn_command command, dir=nil
    dir ||= View.dir
    #dir ||= "tr"
    switch_to_buffer generate_new_buffer("*svn command")
    elvar.default_directory = Bookmarks.expand(dir)
    shell current_buffer
    cm_universal_diff_format
    difflog_highlight
    self.local_keys
    #difflog_highlight
    insert command
    Shell.enter
  end

  def self.diff_dir
    if Line.matches(/^r(\d+)/)
      r = Line.value[/\d+/]
      self.svn_command("echo; svn status; svn diff -x -w -r#{r.to_i - 1}:#{r}")
      #cm_subversion_command "echo; svn status; svn diff -x -w -r#{r.to_i - 1}:#{r}"
      return
    end
    self.svn_command("echo; svn status; svn diff -x -w")
#    cm_subversion_command("echo; svn status; svn diff -x -w")
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
      - Set repository: Repository.url = "http://svn.foo.com/svn/repos/foo"
      - Diff with previous version: Repository.diff(10, nil, "bookmark")
      - Create tag: Repository.tag("tags/v1_0")
      ].strip.gsub(/^      /, '')
  end

  def self.diff_one_file
    if Keys.prefix_u  # If C-u
      cm_compare_one_svn_file
      return
    end
    cm_subversion_command("echo; svn diff -x -w #{file_name_nondirectory(buffer_file_name)}")
  end

  def self.local_keys

#    elvar.repository_mode_map = make_sparse_keymap
      # Inherit notes_mode_map!
    Keys.CN(:cm_diff_map) do
      Line.to_right
      re_search_forward "^Index: "
      Line.to_left
      recenter 0
    end
    Keys.CP(:cm_diff_map) do
      Line.to_left
      re_search_backward "^Index: "
      recenter 0
    end

  end

end
