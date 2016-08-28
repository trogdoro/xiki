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
Ol ""
      return self.quote_selection(:go=>1) if View.selection?

      path = Tree.path
      last = Path.split path[-1]
      line = Line.value
Ol "last", last   #> ["$ pwd"]

      # ^G on "topic > heading" inline, so run as a xiki command...

      if last.length == 1 && last[0] =~ /\A[a-z][a-z0-9 ]+ > \.?[a-z0-9]/i
        Ol "!"
        # return Launcher.go :go=>1
        # View.flash "- Not sure what ^G on 'topic > Heading' should do yet", :times=>5
        return Launcher.launch(:task=>["navigate"])
      end

      # ^G on ":search > heading" inline, so run as a xiki command...

      if last.length == 1 && last[0] =~ /\A:[a-z][a-z ]+ > [a-z]/i
        Ol ""
        View.flash "- Not sure what ^G on ':search > Heading' should do yet", :times=>5
        # return Launcher.go :go=>1
        return
        # return Launcher.launch(:task=>["navigate"])
      end


      # ^G on :search result, so ...

      if last.length == 2 && last[0] =~ /^:[a-z ]+$/i


        if last[1] =~ /^\w/  # || last[-1] =~ /^@\w/
          Ol "How to know what heading to convert it to?!!!"

          Ol "How to get local actions? > do local search? > just expand it without children > not full expand > so it doesn't actionify!!!"
          Ol "Look at installed > and use heading from it!!!"


          # Control G, so just change to heading!
Ol "last", last   #> [":iterm", "brown background"]
          options = {:dont_run_action=>1}
          path = Path.join(last).sub(/^:/, '')
          Ol "path", path   #> "iterm/brown background"
          Expander.expand path, options
          #           options[:heading_found]
          heading = options[:heading_found]
          heading = heading.sub! /(^> @\w+ )/, '> '
          Ol "heading", heading
          # Ol "!!!"

          # Change to heading
          # Line.sub!(/.+/, Line.value.gsub(/( *)[+-] (.+)/){"#{$1}#{heading}"})
          Line.sub!(/^( *)[+-] (.+)/, "\\1#{heading}")

          # Ol "Try > just do search and find one that matches?!!!"

  # (last.length-2).times{ Tree.to_parent }
          # Tree.collapse if Tree.children?
          # Launch if was expanded, leave collapsed if was collapsed
          # Launcher.launch((last.length > 2 ? :change_to_heading_and_launch : :change_to_heading)=>1)
          return ""
        end

        # ^G on "> Heading" under topic, so navigate...

        if last[-1] =~ /^> / || last[-1] =~ /^@\w/
          Ol ""
  # (last.length-2).times{ Tree.to_parent }
  # Tree.collapse if Tree.children?

          # Launch if was expanded, leave collapsed if was collapsed
          # Launcher.launch(:task=>[last.length > 2 ? "as action" : "make action"])

  # return Tree.<<("Todo > Probably toggle back to action!")

          # Change from heading back to path

          path = "foo"
          Line.sub!(/^( *)(.+)/, "\\1+ #{Notes.heading_to_path last[-1]}")

          return ""
        end


        # Ol ""
        # # return Launcher.go :go=>1
        # return Launcher.launch(:task=>["navigate"])
      end


      # ^G under "foo..." (autocomplete), so collapse first...
Ol "last", last   #> ["$ pwd"]
Ol "line", line   #> "$ pwd"
      if last.length == 2 && last[0] =~ /\A[a-z0-9][a-z0-9 ]*\.\.\.\z/
        Tree.collapse_upward :replace_parent=>1
        return self.go_key options
        # Reset vars?
        # path = Tree.path
        # last = Path.split path[-1]
        # line = Line.value
      end

      Ol ""

      # ^G on "topic", and not "topic/", so run as a xiki command...

Ol ""
      if Topic.matches_topic_syntax?(last[0])
Ol ""
        # ^G on "topic" (with no slash at end)
        if last.length == 1 && line !~ /\/$/

          return Launcher.launch(:task=>["view source"])

          # # return Launcher.launch_or_collapse :go=>1 #if last.length == 1
          # Line << "/"
          # Launcher.launch
          # Launcher.launch_or_collapse #:go=>1 #if last.length == 1
          return
        end

        # ^G on topic collapses and > runs with go
        # # ^G on "topic/> Note"
        # if last.length == 2 # && last[1] =~ /^> /
        #
        #   # ^G on > .Foo, so run as action?
        #   # Ol "inserted?!!!"
        #   View.flash "- Not sure what ^G on topic should do yet", :times=>5
        #   return
        # end

      end


#       # if path[-1] =~ /\A\.[a-z][a-z ]*\z/i
#       if last.length == 1 && Topic.matches_topic_syntax?(last[0]) && line !~ /\/$/

# # Ol ":go!!!"
#         return Launcher.go :go=>1 #if last.length == 1

#         # return Launcher.launch(:task=>["navigate"])

#         # raise "Not sure how to deal with nesting more than 2" if last.length > 2

#         #         # If length is 2, assume it's a topic for now

#         #         # /topic/..., might be topic or heading...

#         # Ol "last", last   #> ["ip"]
#         #         if last.length == 2

#         #           # "topic/> Heading", so navigate to it...
#         # Ol "last[1]", last[1]
#         #           if last[1] =~ /^>/
#         #             return Launcher.go :navigate=>1
#         #           end

#         #           # /topic/topic, so collapse first and continue...

#         #           #Tree.collapse_upward #:replace_parent=>1
#         #         end

#         #         # /topic, so run as command
#         #         # Ol "!!"
#         #         Line << "/"
#         #         return Launcher.launch

#       end







            # Ol "path", path
        # => [".hey/", ".docker"]
        # => [".docker"]

            # /topic, so just .expand...

      # if last.length == 1 && last[0] =~ /\A[a-z ]+-\z/i
      # if last.length == 1 && last[0] =~ /\A\.[a-z_ ]+\z/i


      #       if last.length == 1 && Topic.matches_topic_syntax?(last[0])
      # Ol "When is this happening? Should have been captured above!"
      #         Ol "Delegate to > .launch!"
      #         return Launcher.launch
      #       end

      view_dir = View.dir

      # # If just $... (Is this too liberal?)

      # If dir

      # Ol "last", last   #> ["$ pwd"]
      # Ol "path", path   #> ["$ pwd"]

      path_without_filters = path[-1].dup
      FileTree.extract_filters! path_without_filters
      # Ol "!!!"
# Ol "path_without_filters", path_without_filters   #> "$ pwd"
# Ol "path[-1]", path[-1]   #> "$ pwd"

      if FileTree.handles?(path_without_filters)
        # Ol "!!!"
        # /foo/, so just cd to the dir

        # Could be /tmp, "/tmp/$ pwd", or "/tmp/$ pwd/item"

        dir, quote, line = Bookmarks[path_without_filters], nil, nil
        command = nil

        dir.sub!(/\/$/, '')


        # Add quotes unless it's all slashes and letters
        dir_quoted = dir =~ /^[a-z_\/.-]+$/i ? dir : "\"#{dir}\""

        dir_without_snippet, snippet = dir.split("/: ", 2)
        # Ol "dir_without_snippet", dir_without_snippet

        # File
        if File.file? dir_without_snippet
          Ol "dir", dir
          return FileTree.open_with_external_editor Path.unescape(dir)
          # return DiffLog.quit_and_run commands
        end

        # Dir
        if File.directory? dir
          commands = "cd #{dir_quoted}\n"
          # commands << command if command
          return DiffLog.quit_and_run commands
        end

        "- Not file or directory!"
      end

      if last[0] == "markers" && last.length > 1
        # Ol "continue here > continue with > '~ navigate'!!"
        return Tree.<< "- Don't know what ^G on marker means yet!", :no_slash=>1, :hotkey=>1
        # return Launcher.launch :task=>["navigate"]
      end


      # $ ls, so cd to dir or edit file > hard-coded support for ls, for now > abstract out for other commands later...
Ol ""
      # Ol "last", last   # => ["$ ls"]
      if last[0] == "$ ls" && last.length > 1
Ol ""

# dir = last[1..-1].map{|o| o.sub(/^: /, '')}.join("/")
        dir = last[1..-1].map{|o| o.sub(/\/$/, '')}.map{|o| o.sub(/^: /, '')}.join("/")

Ol "dir", dir   #> "usr//share/"

        file = "#{Shell.dir}#{dir}"

        # if dir =~ /\/$/
        if File.directory? file
Ol ""
          commands = "cd #{Shell.quote_file_maybe file}"
        else

          Ol "file", file   #> "/etc"

          FileTree.open_with_external_editor file
          return ""


          # editor = ENV['EDITOR']

          # # Non-console editor, so don't quit
          # if ["subl"].member? editor
          #   command = "#{ENV['EDITOR']} #{view_dir}/#{Shell.quote_file_maybe dir}"
          #   Shell.command command
          #   return
          # end

          # commands = "#{ENV['EDITOR']} #{Shell.quote_file_maybe dir}"
        end

        return DiffLog.quit_and_run commands   #> |||
      end


      # Ol "last", last   # ["topics", "sinatra"]

      # ^G on item under "xiki/", so collapse and run with :go!

Ol "Is it 'topics?':"
Ol "last", last   #> ["$ pwd"]
# if last.length == 2 && last[0] == "tasks"

      if last.length == 2 && last[0] == "xiki"
Ol "!!"

        Xiki[last[1], :task=>["view source"]]
        # Tree.collapse_upward :replace_parent=>1
        # # Line.to_right
        # Line << "/"
        # Launcher.launch_or_collapse
        return ""

        # Manually invoke "* navigate"
        # Xiki["-#{last[1]}", :task=>["navigate"]]
# Xiki["#{last[1]}", :task=>["navigate"]]
        # Hmm, would this be better?:
        # Launcher.launch :task=>["navigate"]


# Xiki[last[1], :task=>["view source"]]


        # Ol "^G on /commands/foo, so replace parent and run command!"

        # Tree.collapse_upward :replace_parent=>1
        # # Line.to_right
        # Line << "/"
        # return Launcher.launch_or_collapse
        # return Launcher.launch_or_collapse :go=>1
      end
Ol ""

      # Xiki command (not $...), so show grab items...

        # => ["ip", "foo"]
        # => [":test"]
Ol "last", last   #> ["$ pwd"]
      if last.length == 1 && last[0] =~ /^\.?[a-z ]+$/i
Ol ""
        # @foo/bar, so it's a xikihub url...

        # if Path.split(path[0]).length == 2

          # + notes/
          # + xiki roots/
          # + shared on xikihub/

          # + save item
          # + save all

          # = local web/
          # = save to xikihub/

        # ^ todo > figure out what to do by default when ^G
        txt = %`
          ^ source
          ^ local web
        `
        # ^ pin to top
        # ^ make newest

        return Tree.<< txt, :no_slash=>1, :hotkey=>1

      end



      # Toggle between "> .Heading" and "+ action"

      #       # ^G on "+ action" under topic (and maybe @foo), so navigate...

      #       if last[1] =~ /^\w/  # || last[-1] =~ /^@\w/
      # Ol ""
      #         (last.length-2).times{ Tree.to_parent }
      #         Tree.collapse if Tree.children?
      #         # Launch if was expanded, leave collapsed if was collapsed
      #         Launcher.launch((last.length > 2 ? :change_to_heading_and_launch : :change_to_heading)=>1)
      #         return ""
      #       end

      # ^G on "> Heading" under topic, so navigate...

      if last[-1] =~ /^> / || last[-1] =~ /^@\w/
        Ol ""
        # (last.length-2).times{ Tree.to_parent }
        # Tree.collapse if Tree.children?

        # # Launch if was expanded, leave collapsed if was collapsed
        # Launcher.launch(:task=>[last.length > 2 ? "as action" : "make action"])
        Launcher.launch :task=>["view source"]
        return ""
      end




      # if last[-1] =~ /^> /
      #   Ol "#{last[0]} #{last[1]}\n"   #> quick > Hey
      #   # Topic.append_log "#{last[0]} #{last[1]}"
      #   return Launcher.launch :task=>["view source"]
      # end


      # ^G on "heading_or_action/@user" under topic, so navigate...


      # ^G on "+ action" under topic, so navigate


      # Todo > make ^G navigate when "+ action"?
      # :go doesn't work. Pass new option? > or make :go work?
      # Or > use "^ go options"? > if we find a use for them

      # return Launcher.launch(:go=>1)
      # return Launcher.launch((last.length > 2 ? :change_to_heading_and_launch : :change_to_heading)=>1)


      # Old
      # Assume /:topic/topic
      # Tree.collapse_upward :replace_parent=>1
      # return Launcher.launch



      # if last.length >= 2
      #   is_topic_action = Topic.matches_topic_syntax?(last[0]) && Topic.matches_topic_syntax?(last[1])
      #   is_topic_heading = Topic.matches_topic_syntax?(last[0]) && last[1] =~ /^> /
      # end

      # # topic/action, so turn back into heading...

      # if is_topic_action

      #   (last.length-2).times{ Tree.to_parent }
      #   Tree.collapse if Tree.children?

      #   # Launch if was expanded, leave collapsed if was collapsed
      #   Launcher.launch((last.length > 2 ? :change_to_heading_and_launch : :change_to_heading)=>1)

      #   return ""
      # end

      # # "topic/> Heading", so do "~ as action"
      # if is_topic_heading

      #   (last.length-2).times{ Tree.to_parent }
      #   Tree.collapse if Tree.children?

      #   # Launch if was expanded, leave collapsed if was collapsed
      #   Launcher.launch(:task=>[last.length > 2 ? "as action" : "make action"])
      #   return ""
      # end


      # Blank line, so say to use ^O instead...

Ol "last", last   #> ["$ pwd"]

      if last == []
Ol ""
        # View.flash "Try Ctrl+O or Ctrl+W on blank lines"
        View.flash "Try Ctrl+T or Ctrl+X on blank lines"
        return ""
      end


      # Not a shell command, so do nothing...

      if last[-1] !~ /^[$%&] (.+)/
        Ol ""
        # View.flash "- Not sure what ^G should do with this!"
        Launcher.launch :go=>1
        return ""
      end

Ol ""
      # Shell command in any ancestor, so run back in bash...

      command = $1
      ancestors = options[:ancestors] || Tree.ancestors

      # /dir/$..., so grab command and dir...

      # if ancestors && FileTree.handles?(ancestors[-1])
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
Ol ""

      # $..., so grab command, and figure out the dir...


      # Old > Not sure if this was necessary
      # # They cd'ed in this session > so do cd in the shell...

      # session_dir = File.expand_path Bookmarks.bookmarks_optional("se")
      # in_session_dir = view_dir == session_dir

      # # If session, do based on view dir
      # orig_dir = Xsh.determine_session_orig_dir(View.file_name) if in_session_dir

      # # If couldn't find, or normal dir, default to current dir

      # orig_dir ||= view_dir
      # orig_dir.sub! /\/$/, ''   # To ensure a fair comparison

      # # If it's a view without a file, always do cd
      # if ! View.file || orig_dir != dir   # Or if they changed the dir


      # They did a cd, so add it...

Ol "!!!"
Ol "command", command   #> "pwd"
      self.prepend_cd_if_changed command
Ol "command", command   #> "pwd"

#       dir = Shell.dir
#       dir.sub!(/\/$/, '') if dir != "/"

# Ol "dir", dir   #> "/Users/craig"

#       Ol "expand, so it's not '~'!!"
#       if dir != View.app_dir
#         dir = "\"#{dir}\"" if dir !~ /^[a-z_\/.-]+$/i
# Ol "!!!"
#         command.replace "cd #{dir}\n#{command}"
# # command = "cd #{dir}\n#{command}"
#         # commands << "cd #{dir}\n"
#       end


      DiffLog.quit_and_run "#{command}\n"   #> |||
      # DiffLog.quit_and_run commands

    end


    def self.prepend_cd_if_changed command, options={}
Ol.stack

      dir = options[:dir] || Shell.dir
      dir.sub!(/\/$/, '') if dir != "/"

Ol "dir", dir   #> "/tmp"

Ol "View.app_dir", View.app_dir   #> "/tmp"
      Ol "expand, so it's not '~'!!"
      if dir != View.app_dir
        dir = "\"#{dir}\"" if dir !~ /^[a-z_\/.-]+$/i
Ol "!!!"
        command.replace "cd #{dir}\n#{command}"
# command = "cd #{dir}\n#{command}"
        # commands << "cd #{dir}\n"
      end

    end



    def self.save options
      # Saves a note
Ol "!!!"

      # No "> Foo" at beginning, so prompt them to type it and do nothing
      return if Notes.prompt_for_heading_if_missing

      # Convert to 2 level indenting
      Notes.indent_note_under_heading

      # A heading exists, so convert to 2-level indenting, then save...

      # Delegate to ~ save
      Launcher.launch(:task=>["save"])

      # "Todo > finish"
      ""
    end


    def self.content_editor

      # Untitled view, so save file as session before opening in editor
      DiffLog.save_xsh_interaction


      if ! View.file
        Ol "!!!"
        return
      end


      View.auto_revert
      FileTree.open_with_external_editor(View.file, :line=>View.line)
    end


    # def self.share options

    #   # No "> Foo" at beginning, so prompt them to type it and do nothing
    #   return if Notes.prompt_for_heading_if_missing


    #   # Convert to 2 level indenting
    #   Notes.indent_note_under_heading

    #   Ol "todo > .share > implement!"

    #   # Delegate to ~ save
    #   Launcher.launch(:task=>["share"])


    # end

  end
end
