class Rbenv

  MENU = %q`
    - .install/
    - .uninstall/
    - files/
      =~/.rbenv/
        + shims/
        + versions/
        + version
    - docs/
      - set version/
        - globally/
          =% rbenv global 1.9.3-p484
        - per project/
          =/tmp/myproject/
            % rbenv local 1.9.3-p484
        - per shell/
          =/tmp/
            % rbenv shell 1.9.3-p484
      - update shims/
        =% rbenv rehash
    =notes/
    `

  def self.menu_after output, *args

    dir = Tree.dir
    global_or_local = dir ? "local" : "global"

    # /, so prepend rubies...

    if args == []

      txt = Shell.sync "rbenv versions", :dir=>(dir||"/tmp/")

      txt.gsub!(/ *(.+)/, ": \\1")

      return ": Expand to enable #{global_or_local}ly:\n"+txt+output
    end

    # /output from MENU, so do nothing...

    return nil if output

    # /version, so enable it?...

    version = self.clean_version args[0]

    # If ancestor dir set locally, if no parent dir set globally

    Shell.sync "rbenv #{global_or_local} #{version}", :dir=>(dir||"/tmp/")
    global_or_local == "local" ? "<! created .ruby-version file!" : "<! updated ~/.rbenv/version!"

  end

  # Rbenv.clean_version! ": * 1.9.3-p327 (set by /Users/craig/.rbenv/version)"
  #   1.9.3-p327
  def self.clean_version version
    version.sub!(/^: /, '')
    version.sub!(/^\* /, '')   # Remove preceding "* "
    version.sub!(/ \(.*/, '')   # Remove trailing " (set by...)"
    version
  end


  def self.install version=nil
    # /, so list all...

    if ! version
      txt = Shell.sync "rbenv install --list", :dir=>"/tmp/"
      txt.sub!(/^\AA.+\n/, '')   # Remove label at top
      txt.gsub!(/ *(.+)/, "- \\1/")
      txt = "=notes/rbenv/> Get rbenv to show latest ruby versions\n#{txt}"
      return txt
    end

    # /version, so install it...

    Shell.async "rbenv install #{version}", :dir=>"/tmp/"
    ""
  end

  def self.uninstall version=nil
    # /, so list all...

    dir = Tree.dir || "/tmp/"

    if ! version
      txt = Shell.sync "rbenv versions", :dir=>dir

      txt.gsub!(/ *(.+)/, ": \\1")
      return txt
    end

    # /version, so install it...

    version = self.clean_version version
    Shell.async "rbenv uninstall #{version}", :dir=>dir#,       :mock=>1
    ""
  end


end
