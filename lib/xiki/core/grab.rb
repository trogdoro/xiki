module Xiki
  class Grab
    def self.call_grab_method options

      first = options[:grab_option][0]

      self.send(TextUtil.snake(first), options)
    end

    def self.options options
      Tree.collapse
      Launcher.launch :task=>[]#, :hotkey=>1
      ""
    end

    def self.add options

      line = Line.value
      options_in = {}

      # At left margin or equals sign
      if Line.indent(line) == "" || line =~ /^ *=/
        # so indenting under is fine
      else
        # Otherwise, over-ride indent to be same as current line
        options_in[:no_indent] = 1
      end

      Notes.add_note_prompt options_in

      ""
    end


    def self.source options
      Command.to_menu
      nil
    end

    def self.hide options
      # Do nothing, siblings should be already hidden?
      nil
    end

    def self.quote_selection options={}

      View.deselect
      txt = View.delete *View.range

      # Already quoted, at left margin, so just unquote...

      if txt =~ /\A *\|/
        txt.gsub!(/^( *)\| ?/, "\\1")

        View.<< txt, :dont_move=>1
        return
      end

      # Not quoted, so quote it...

      indent = txt[/\A */]

      txt = txt.gsub(/^#{indent}/, "\\0| ")
      txt.gsub!(/^( *\|) $/, "\\1")   # Remove trailing linebreaks

      if ! options[:go]
        txt.gsub! /^/, "  "
        txt = "\n#{txt}"
      end

      View.<< txt, :dont_move=>1
      View.remove_last_undo_boundary

    end

    def self.to_grab_target_view txt

      # Var wasn't set, so tell caller to continue...

      return false if ! $el.boundp(:grab_target_view)

      grab_target_view = $el.elvar.grab_target_view

      # Var was set, so jump to target view and insert there...

      View.kill
      View.to_buffer grab_target_view

      # Make blank line after, if stuff on this line

      if ! Line.blank?
        Line.to_right
        View << "\n"
      end

      View.<< txt.sub(/\n+\z/, "")
      true   # Tell caller we handled it
    end


    def self.go_key options={}
      return self.quote_selection(:go=>1) if View.selection?

      path = Tree.path
      last = Path.split path[-1]
      line = Line.value

      # ^G on "topic > heading" inline, so run as a xiki command...

      if last.length == 1 && last[0] =~ /\A[a-z][a-z0-9 ]+ > \.?[a-z0-9]/i
        return Launcher.launch(:task=>["navigate"])
      end

      # ^G on ":search > heading" inline, so run as a xiki command...

      if last.length == 1 && last[0] =~ /\A:[a-z][a-z ]+ > [a-z]/i
        View.flash "- Not sure what ^G on ':search > Heading' should do yet", :times=>5
        return
      end


      # ^G on :search result, so ...

      if last.length == 2 && last[0] =~ /^:[a-z ]+$/i


        if last[1] =~ /^\w/  # || last[-1] =~ /^@\w/

          # Control G, so just change to heading!
          options = {:dont_run_action=>1}
          path = Path.join(last).sub(/^:/, '')
          Expander.expand path, options
          heading = options[:heading_found]
          heading = heading.sub! /(^> @\w+ )/, '> '

          # Change to heading
          Line.sub!(/^( *)[+-] (.+)/, "\\1#{heading}")

          return ""
        end

        # ^G on "> Heading" under topic, so navigate...

        if last[-1] =~ /^> / || last[-1] =~ /^@\w/

          # Change from heading back to path

          path = "foo"
          Line.sub!(/^( *)(.+)/, "\\1+ #{Notes.heading_to_path last[-1]}")

          return ""
        end
      end


      # ^G under "foo..." (autocomplete), so collapse first...

      if last.length == 2 && last[0] =~ /\A[a-z0-9][a-z0-9 ]*\.\.\.\z/
        Tree.collapse_upward :replace_parent=>1
        return self.go_key options
      end

      # ^G on "topic", and not "topic/", so run as a xiki command...

      if Topic.matches_topic_syntax?(last[0])

        # ^G on "topic" (with no slash at end)
        if last.length == 1
          return Launcher.launch(:task=>["view source"])
        end
      end

            # /topic, so just .expand...

      view_dir = View.dir


      path_without_filters = path[-1].dup
      FileTree.extract_filters! path_without_filters

      if FileTree.handles?(path_without_filters)

        # /foo/, so just cd to the dir

        # Could be /tmp, "/tmp/$ pwd", or "/tmp/$ pwd/item"

        dir, quote, line = Bookmarks[path_without_filters], nil, nil
        command = nil

        dir.sub!(/\/$/, '')


        # Add quotes unless it's all slashes and letters
        dir_quoted = dir =~ /^[a-z_\/.-]+$/i ? dir : "\"#{dir}\""

        dir_without_snippet, snippet = dir.split("/: ", 2)

        if File.file? dir_without_snippet
          return FileTree.open_with_external_editor Path.unescape(dir)
        end

        # Dir
        if File.directory? dir
          commands = "cd #{dir_quoted}\n"
          return DiffLog.quit_and_run commands
        end

        "- Not file or directory!"
      end

      if last[0] == "markers" && last.length > 1
        return Tree.<< "- Don't know what ^G on marker means yet!", :no_slash=>1, :hotkey=>1
      end

      # $ ls, so cd to dir or edit file > hard-coded support for ls, for now > abstract out for other commands later...

      if last[0] == "$ ls" && last.length > 1

        dir = last[1..-1].map{|o| o.sub(/\/$/, '')}.map{|o| o.sub(/^: /, '')}.join("/")

        file = "#{Shell.dir}#{dir}"

        if File.directory? file
          commands = "cd #{Shell.quote_file_maybe file}"
        else

          FileTree.open_with_external_editor file
          return ""
        end

        return DiffLog.quit_and_run commands   #> |||
      end


      # ^G on item under "xiki/", so collapse and run with :go!

      if last.length == 2 && last[0] == "xiki"

        Xiki[last[1], :task=>["view source"]]
        return ""

      end

      # ^G on "> Heading" under topic, so navigate...

      if last[-1] =~ /^> / || last[-1] =~ /^@\w/
        Launcher.launch :task=>["view source"]
        return ""
      end


      # Blank line, so say to use ^O instead...

      if last == []
        View.flash "Try Ctrl+T or Ctrl+X on blank lines"
        return ""
      end

      # Not a shell command, so do nothing...

      if last[-1] !~ /^[$%&] (.+)/
        Launcher.launch :go=>1
        return ""
      end

      # Shell command in any ancestor, so run back in bash...

      command = $1
      ancestors = options[:ancestors] || Tree.ancestors

      # /dir/$..., so grab command and dir...

      (ancestors||[]).reverse.each do |ancestor|
        # Only continue if it's a file path
        next if ! FileTree.handles?(ancestor)

        dir = Bookmarks[ancestor]
        dir.sub! /\/\/.*/, "/"   # Under menufied, so change "/foo//bar" to "/foo/" before cd'ing

        dir = Shell.quote_file_maybe dir

        # Todo > Do this, when there are args?
        #           # Pop off any others at end that are $... or ~...
        #           while last[-1] =~ /^[~$%&]/
        #             last.pop
        #           end

        commands = "cd #{dir}\n#{command}"
        return DiffLog.quit_and_run commands

      end

      # $..., so grab command, and figure out the dir...

      self.prepend_cd_if_changed command

      DiffLog.quit_and_run "#{command}\n"   #> |||
    end


    def self.prepend_cd_if_changed command, options={}

      dir = options[:dir] || Shell.dir
      dir.sub!(/\/$/, '') if dir != "/"

      if dir != View.app_dir
        dir = "\"#{dir}\"" if dir !~ /^[a-z_\/.-]+$/i
        command.replace "cd #{dir}\n#{command}"
      end

    end


    def self.save options
      # Saves a note

      # No "> Foo" at beginning, so prompt them to type it and do nothing
      return if Notes.prompt_for_heading_if_missing

      # Convert to 2 level indenting
      Notes.indent_note_under_heading

      # A heading exists, so convert to 2-level indenting, then save...

      # Delegate to ~ save
      Launcher.launch(:task=>["save"])

      ""
    end


    def self.content_editor

      # Untitled view, so save file as session before opening in editor
      DiffLog.save_xsh_session

      if ! View.file
        return
      end

      View.auto_revert
      FileTree.open_with_external_editor(View.file, :line=>View.line)
    end


  end
end
