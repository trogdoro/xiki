class Git
  def self.status dir
    # If no file, show status
    txt = Shell.run("git status", :sync => true, :dir => dir)
    self.status_internal(txt)
  end

  def self.status_internal txt
    txt.gsub!(/^#\t(.+?): +/, "- \\1: ")
    txt.gsub!(/^#\t/, "- ")
    txt.gsub!(/^#\n/, '')
    txt.gsub!(/^#/, '|')
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

end
