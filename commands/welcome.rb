# Special case when 'xsh' not in path yet...

# /, so just say welcome or suggest adding to path...
if args == []
  if options[:add_xiki_to_path]
    return "
      | Welcome to Xiki! It appears you haven't made the 'xsh'
      | shell command available system-wide yet. If you want to,
      | expand the following command (use the arrow keys to move
      | your cursor to it, then type Ctrl+E):
      |
      + add xiki to path/
      ".unindent
  else
    return "
      | Welcome to Xiki!
      | todo > add more here
      "
  end
end

# /add xiki to path, so run it...

if args[0] == "add xiki to path"
  line_to_add = "export PATH=$PATH:#{Bookmarks[':xiki']}bin"

  # Use the bash rc file that actually runs (bash only runs one of them, in this order)
  conf_file = ["~/.bash_profile", "~/.bash_login", "~/.profile"].find{|o| File.exists? File.expand_path(o)}

  return "
    | Adding this line to your shell config well let you type
    | 'xsh' in any directory. Expand 'continue' below to do so
    | (use arrow keys and Ctrl+E):
    #{conf_file}
      :+#{line_to_add}
    |
    + continue
    " if ! args[1]

  # /continue, so add it and say use --help...

  if args[1] == "continue"

    file = File.expand_path(conf_file)
    txt = File.read file

    # Line already in file, so error and remind them that it has to be a new session

    return "
      |-The line already exists in #{conf_file}
      |
      | You may need to close this shell session (and other open
      | shell sessions) and start a new one, so the change to
      | #{conf_file} has a chance to take effect.
      " if txt[line_to_add]

    # Append it to the contents of .bash_login
    line_to_add = "
      # Add 'xsh' to the path, so you can run it from any directory
      #{line_to_add}
      ".unindent
    txt.sub! /\n+\z/, "\n\n#{line_to_add}"

    File.open(file, "w") { |f| f << txt }

    return "
      =replace/siblings/2/
        |+Success!
        |
        | Your #{conf_file} file has been updated! You can now type
        | 'xsh' from any directory (be sure to close any existing
        | shell sessions before trying it).
        |
        | Type 'xsh --help' on the command line for help using xsh.
      ".unindent

  end

end



__END__

> Welcome to Xiki
| Here are some menus and links to get you started.  Double-click
| on them or type control-enter (or command-enter) while the cursor
| is on them.

+ =docs/

> Website and Screencasts
| Check out the screencasts on xiki.org:
=http://xiki.org
=http://xiki.org/screencasts

> Twitter and google group
=http://twitter.com/xiki
=http://groups.google.com/group/xiki/

> Stop showing this menu on startup
+ =xiki/setup/misc/dont show welcome/
