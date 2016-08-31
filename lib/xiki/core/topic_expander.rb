module Xiki
  class TopicExpander

    def self.exempt_commands
      ["search", "share"]
    end

    def self.expands? options

      name = options[:name]   # "slider"

      items = options[:items]   # ["> Clear"]

      return if name.blank?

      return if ! Topic.matches_topic_syntax?(name)

      # Add ourself as expander
      (options[:expanders] ||= []).push(self)
    end

    # Create intermediate structure for topic results. It contains notes and commands that match the topic.

    def self.search_local_structure structure, options={}

      # For each key add hash to value

      structure.each do |key, val|
        txt = File.read(val[1])
        txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

        filter = val[0].join " "
        val << Notes.wiki_headings_to_hash(txt, options.merge(:filter=>/\b#{Regexp.escape filter}/i))
      end

    end


    def self.local_structure name

      name = Topic.remove_topic_syntax name

      filter = name.split "_"

      structure, name = {}, []

      # Make intermediate structure to use to build output with later...

      # Ie. "foo##bar bah", "foo bar##bah", "foo bar bah"
      # { "foo"=>
      #   [["bar", "bah"], "notes filename", {optional array of headings or hash of headings and content}]
      # }

      while filter.any?    # while still filter words left to append to word
        name << filter.slice!(0)   # name: "foo", filter: "bar bah"

        name_spaced, name_underscored = name.join(' '), name.join('_')

        # Finds foo.xiki or foo.link if it exists
        file = Dir["#{File.expand_path("~")}/xiki/#{name_underscored}.*"][0]

        # Don't add item if > no file named like this word permutation
        next if ! file

        file = Notes.expand_if_link file

        structure[name_spaced] = [filter.dup, file]

      end

      structure
    end

    # No heading passed, so do main topic search
    def self.expand_noargs options

      name, task, items, go = options[:name], options[:task], options[:items], options[:go]
      name_spaces = name.gsub("_", ' ')

      Topic.append_log "#{name_spaces}" if ! XIKI_SERVER_MODE

      if go
        # ^G on root, so use go opener if any
        if output = self.delegate_to_go_opener(options)   #> |||
          return output
        end


        # ^G on root and no go opener, so use snippets opener if any
        # No :go go, so delegate to :snippets opener if any
        if output = self.delegate_to_snippet_opener(options)
          return output
        end

      end

      structure = self.local_structure options[:name]   # noargs

      # Turn structure into output

      headings, notes, commands = {}, "", ""

      # "* option item" exist, so pick just one notes file, and make it process the option...

      if task

        single_topic = structure == {} ? nil : structure.values[-1][1]

        # if
          # single_topic = nil
        # single_topic = structure.values[-1][1]
          # Pick first of my private notes
          # Find last one, since that'll match more words in the topic (for multi-word topics)

        # "* view source" on new command, so open new view
        if task == ["view source"]

          file = Dir["#{File.expand_path("~")}/xiki/#{name.snake_case}.*"][0]
          file = Notes.expand_if_link file

          # No file exists yet, so default to normal filename
          file ||= "~/xiki/#{name.snake_case}.xiki"

          View.open file

          View.flash("- New file", :dont_nest=>1) if ! File.exists?(file)
          return ""
        end

        # Otherwise, pick first hub note, and have Notes.drill draw the optien items...

        # Todo > This should probably be narrowed down to make sure it's mine?

        if single_topic
          return Notes.drill(single_topic, options)
        else
          # Note not found, so just pass name > it'll assume a nonexisting filename
          return Notes.drill(name, options)
        end
      end

      if ! options[:tab] && ! options[:go]

        # Go through each note file and extract headings...

        structure.each do |key, row|   #> |||||||
          filter, file = row
          filter = filter.join " "

          # Extract headings from each file

          options_in = {:inline_warning_messages=>1}
          Options.propagate_some_inward options, options_in

          # ?? > the enclosing 'if' block is only entered when > "! options[:go]"

          options_in[:go] = 1 if options[:go]# || options[:ctrlx]

          # Todo later > deal with issue > if ^T > it may show options multiple times > once for each potential item

          list = Notes.drill(file, options_in)   #> ||||||||

          list = list.unindent.split("\n")

          if filter.any?
            # Make sure it starts with the string (leave end ambiguous, to match "foo" or "foos" etc)
            list = list.select{|o| o =~ /\b#{filter}/i}
          end

          kind, user, topic = file.match(/(\w+)\/([\w.-]+)\/(\w+)\.(xiki|notes|link)$/)[1..3]

          next if ! list.any?

          headings["#{kind}/#{user}/#{topic}"] ||= []
          headings["#{kind}/#{user}/#{topic}"] += list

        end

      end

      # No headings nor exact match commands, just do tab completion...

      if !task && !go && (name.length == 1 || headings == {}) # && ! top_topics)
        options[:line_found] = 2
        return ": None found.\n* add note"
      end

      # Turn into one big string
      txt = ""
      headings.keys.reverse.each do|k|
        v = headings[k]

        if options[:include_extra_info]
          kind, user, topic = k.split("/")
          next txt.<< v.map{|o| "@#{user}/#{topic}/#{o}\n"}.join("")
        end

        txt << v.map{|o| "#{o}\n"}.join("")
      end

      headings = txt


      # Remove "@" and "@usernames"
      headings.gsub! /^> @\w* /, "> "
      headings.gsub! /^\+ @\w* /, "+ "

      # Make unique across all headings
      headings = headings.split("\n").uniq.map{|o| "#{o}\n"}.join("")

      results = headings

      # :go, so append command output...

      if go

        if Command.exists?(name) || Command.defs[name]
          command_output = Xiki.expand("`#{name}", options) || ""
        end

        # If command and no output > ignore action items

        return if command_output == ""

        # Unindent if needed
        command_output = command_output.unindent if command_output =~ /\A[ \n]/

        # ^T, so ignore actions..

        return command_output if task

        # Only add "\n" between if any results there

        results = "#{command_output.sub(/\n+\z/, '')}\n#{results}" if command_output
        return results
      end

      # redundant call here > don't call if > did ^O!
      return self.tasks_dropdown(options) if !task && results == ""

      # Set :line_found Based on the original heading!
      if original_heading = options[:original_heading]

        if original_heading =~ /> (@\w* )?(\..+)/
          original_heading = "+ " + Notes.heading_to_path(original_heading, :remove_username=>1)   #> Check!!!
        end

        index = results.index(/^#{Regexp.quote original_heading}$/)

        if index
          linebreaks_above = results[0..index].scan("\n").count + 1
          options[:line_found] = linebreaks_above
        end
      end

      options[:digit_means_replace_parent] = 1

      results
    end

    def self.expand options

      name, task, items = options[:name], options[:task], options[:items]

      options[:go] = 1 if options[:ctrlx]

      # Was a http://url, so don't intercept
      # Better solution > make the :halt option get propagated back from http... pattern   #> ||||||||||||
      #   - then do nothing if set
      #   - (:halt currently isn't making it back)
      # return if options[:name] == "http:" && items && items[0] == ""

      # Return if something already handled
      return if options[:output] || options[:halt]

      # topic (no heading or items), so delegate...

      return options[:output] = self.expand_noargs(options) if ! items

      # topic/foo...

      # ^T on topic/foo, so delegate...
      return options[:output] = self.tasks_dropdown(options) if ! items && task

      # topic/quote, so pass to 'root' item...

      if items[0] =~ /\n/

        return if self.delegate_to_snippet_opener(options)

        # For now > Always assume root exists   #> {}
        # After porting roots > Remove this line > Say 'Not found' Instead

        return options[:output] = Xiki.expand("`#{options[:name]}", options)   #> |||||||
      end

      # Get content if one version of task, or usernames if multiple versions (@nov, @private, etc.)...

      structure = self.local_structure options[:name]
      heading = items[0]

      # Find versions, including installed from users, private, and shared
      # Returns keys like "@private", "@shared", and "@nov"
      found_by_user = self.found_by_user structure, heading

      # Heading not found, so user is trying to create...

      if heading =~ /^> / && found_by_user.empty? && ! task

        # Just expanded heading, so show space for them to create...

        content = items[1]

        # No content passed in, so return handle option items

        if ! content
          return options[:output] = "* add note\n* web search" if task == []   # Give option that's the same as the default behavior

          if task == ["add note"]
            indent = Line.indent
            Line.<< "\n#{indent}  | \n#{indent}  | ", :no_search=>1
            Move.previous
            Line.to_right
            View.prompt "add note", :no_slash=>1
            return options[:output] = ""
          end

          if task == ["web search"]
            txt = "#{name} #{items.join(' ')}".gsub(" >", "")
            Xiki::Google.search txt, :via_os=>1
            return options[:output] = ""
          end

          return options[:output] = "<* Task not recognized" if task
        end

        file = nil
        username = XikihubClient.username || "me"

        return options[:output] = "<* - Doesn't exist. Try Ctrl+T" if structure == {}

        # Content passed in, so use other existing file or topic name as file path...

        file ||= structure.values[-1][1]   # Last of my notes files that existed

        file ||= "~/xiki/#{name}.xiki"

        # Delegate to Notes.drill, which will walk user through creating

        txt = Notes.drill(file, *items, options)   #> |||||||||||
        txt = "<* - Not found!" if ! txt
        return options[:output] = txt
      end

      # Username was passed in, so delegate to just that file...

      # Probably do this in the future > require "topic/action/@user" > when dups for actions
      # For now, only worry about duplicates when > "> Heading"

      if items.length >= 2 && items[1] =~ /\A@\w/ && items[1] !~ /\n/

        user = items[1]
        items.slice! 1   # Remove username from path

        # Insert username into heading, so it'll find specific one
        self.put_username_into_heading items[0], user

        file = found_by_user[user]

        # @user passed in path was just upvote, so get from server

        if file.is_a? Array
          command = file[1][/(\w+)\.xiki$/, 1]
          user = user[/@(\w+)/, 1]
          heading = items[0].sub(/@\w+ /, "")

          # Text passed said delegate to search for navigating
          if items[-1] =~ /\n/
            path = Path.join [":#{command}", heading, "@#{user}", items[-1]]
            Xiki[path]
            return options[:output] = ""
          end

          txt = XikihubClient.task([user, command, heading])

          options[:line_found] = 2
          return options[:output] = ": @#{user}, upvoted\n#{txt}"
        end

        txt = Notes.drill(file, *items, options)   #> |

        # If :go, .drill would have navigated to it, I think
        return "" if options[:go]

        # Expand dynamic content of action...

        heading_found = options[:heading_found]

        if items[0] !~ /^> / && heading_found =~ /^> (@\w* )?\./   #> "> @ .One shared"
          txt = Tree.unquote txt
          items.shift if items

          return options[:output] = self.expand_action(txt, file, options)   #> ||
        end

        # Add 'installed'
        if heading_found =~ /^> @\w+ /   #> "> @ .One shared"
          txt = ": installed from #{user}\n#{txt}"
          options[:line_found] = 2
        end
        return options[:output] = txt

      end

      users = found_by_user.keys

      # Multiple users, so show each user...

      if users.length > 1
        users = users.map{|o| "#{o}\n"}.join('')
        return options[:output] = users
      end

      # Only one user, so just delegate to .drill...

      # Set path of note file (hub/ dir if "* shared")...

      if task == ["share"]
        Ol "Todo > how to share? > change heading to '> @ Foo' > then sync?!"
      else
        file = found_by_user[users[0]]
      end

      # The single user name is an upvote > so get task from server

      if file.is_a?(Array)
        # Remove this after merging single user into > Above handling username past in in path
        command = file[1][/(\w+)\.xiki$/, 1]
        user = users[0][/@(\w+)/, 1]
        heading = items[0] #.sub("@ ", "")

        # Text passed said delegate to search for navigating
        if items[-1] =~ /\n/
          path = Path.join [":#{command}", heading, "@#{user}", items[-1]]
          Xiki[path]
          return options[:output] = ""
        end

        options[:line_found] = 2
        txt = XikihubClient.task([user, command, heading])   #> Error > undefined method `sub!' for nil:NilClass

        return options[:output] = ": @#{user}, upvoted\n#{txt}"
      end

      # Nothing found, so delegate to go opener if any
      if ! file

        if output = self.delegate_to_go_opener(options)   #> |||
          return options[:output] = output
        end
      end

      if ! file && items[0] !~ /^> /
        return options[:output] = Xiki.expand("`#{options[:name]}", options) || ""   #> ||||||
      end

      # Insert username into heading, so it'll find it

      self.put_username_into_heading items[0], found_by_user.keys[0]

      # Non-existing file, so create
      file ||= "~/xiki/#{name}.xiki"

      txt = Notes.drill(file, *items, options)   #> |||

      # topic/path, so expand as action, or insert unquoted (if normal heading)

      if items[0] !~ /^> /   # The line we're expanding should be a "+ path", not a "> .Heading"

        # :dont_run_action, so Just return
        return if options[:dont_run_action]

        # topic/path matched "> .action", so call .expand_action (eval or delegate to command)...

        heading_found = options[:heading_found]
        heading_found = Notes.remove_ignorable_heading_parens heading_found

        if heading_found =~ /^> (@\w* )?\./   #> "> @ .One shared"
          # Heading didn't have a dot, so just show literally

          # Add action to history
          Topic.append_log "#{name.gsub('_', ' ')}/#{items.join("/")}"

          txt = Tree.unquote txt

          items.shift if items

          return options[:output] = self.expand_action(txt, file, options)   #> ||
        else
          # topic/path matched "> note", so unquote under equals...

          txt = Tree.unquote txt

          return options[:output] = txt

        end
      end

      # Just return result if "{..." or not a string
      return options[:output] = txt if ! txt.is_a?(String) || txt =~ /\A\{/

      if items.length == 1 && (! task || task == ["view source"]) && ! XIKIHUB_DIR
        Topic.append_log "#{name.gsub('_', ' ')} #{items[0]}"
      end

      # Just a heading passed, so maybe add some stuff to output

      if items.length == 1 && ! task

        label_regex = XikihubClient.comment_or_upvote_label_regex "\\w+"
        label = Tree.unquote(txt)[/#{label_regex}/]

        if users[0] == "@shared" && label =~ /^upvote/i
          # Single upvote line, so get actual contents from xikihub...

          raise "This will probably never happen now, so remove This code? > Because > We now parse the upvoted User name in the code above"

          command = file[/(\w+)\.xiki$/, 1]
          user = label[/@(\w+)/, 1]
          heading = items[0].sub("@ ", "")

          txt = XikihubClient.task([user, command, heading])   #> Error > undefined method `sub!' for nil:NilClass

          return options[:output] = ": @#{user}, upvoted\n#{txt}"
        end

        # Show who's it is on first line
        if users[0] != "@private"
          # txt = users[0] == XikihubClient.username ?
          txt = users[0] == "@shared" ?
            ": Your shared version\n#{txt}" :
            ": installed from #{users[0]}\n#{txt}"
          options[:line_found] = 2
        end

      end

      options[:output] = txt

    end


    def self.delegate_to_go_opener options

      name = options[:name]
      name_spaces = name.gsub("_", " ")

      xiki_file = Topic.topic_to_filename name
      txt = File.read xiki_file rescue nil   #> Extract > self.invoke_expander_from_pattern__!
      return false if ! txt

      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      # Look for :snippets expander pattern   #> nil

      # Figure out action that matches
      patterns = Topic.extract_opener_patterns txt

      # heading = patterns["#{name_spaces}/:go"]
      heading = patterns["#{name_spaces}/:slash"]

      return false if ! heading

      # Heading found, so run it
      user = TopicExpander.username_from_heading heading
      path = Notes.heading_to_path heading, :remove_username=>1

      # Delegate to action!

      args_in = [path, user]
      args_in += options[:items] if options[:items]

      Options.propagate_important_options(options) do |options|
        # Options is now limited to ones that should be propagated downward
        Xiki.expand(name_spaces, args_in, options)   #> hello
      end

    end


    def self.delegate_to_snippet_opener options

      name, items = options[:name], options[:items]

      xiki_file = Topic.topic_to_filename name
      txt = File.read xiki_file rescue nil
      return false if ! txt

      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      # Figure out action that matches
      patterns = Topic.extract_opener_patterns txt

      # Delegate to expander

      # Look for :snippets expander pattern

      heading = patterns["#{name}/:snippet"]

      return false if ! heading

      # Heading found, so run it
      user = TopicExpander.username_from_heading heading
      path = Notes.heading_to_path heading, :remove_username=>1

      # Delegate to action!

      args_in = [path, user]
      args_in << items[0] if items

      result = nil
      Options.propagate_important_options(options) do |options|
        # Options is now limited to ones that should be propagated downward

        result = Xiki.expand(name, args_in, options)   #> hello
      end

      result ||= ""

      options[:output] = result

      # Indicate we handled it
      result

    end


    def self.put_username_into_heading heading, username

      return if ! username
      return if username == "@private"
      username = "@" if username == "@shared"

      if heading =~ /^> /
        heading.sub! /^> /, "> #{username} "
      else
        heading.sub! /^/, "#{username} "
      end

    end


    #
    # > .Test blank_out_wrapper_patterns
    # xiki api/blank out wrapper patterns
    #
    def self.expand_action txt, file, options   #> ||||-

      # Remove any "# foo\n  |:..." wrapper patterns from top

      Notes.blank_out_wrapper_patterns txt

      # We only get called if txt was found in the note action.   #> !
      # If action wasn't there, it would have already delegated to the command.
      if txt =~ /\A\n*!( |$)/

        # "!...", so just eval...

        # If any blank line after !... line, remove everything after it   #> Might cause problems!
        txt.sub!(/(^! [^\n]+)\n\n.+/m, "\\1\n")

        txt.gsub!(/^! ?/, "")   #> ["red"]

        options[:args] = options[:items]#[1..-1]   #> []

        txt = Code.eval txt, file, options[:heading_line_number]+1, {:pretty_exception=>1, :simple=>1}, options   #> ||||||
        return txt

      elsif txt =~ /\A\& /
        # Remove all lines but first
        command = txt[/\A(\& .+)/, 1]

        # "& foo", so run shell command back in bash...
        # Remove all lines but first

        command = txt[/\A\& (.+)/, 1]

        # Only if not in ~/xiki > Change into that projects directory
          # Probably change this
            # Because maybe people share commands in a dropbox or other private directory > They should be able to run commands in the directory where the user was
            # Have alternate syntax for doing it in the current directory
              # ./
              #   & Run this in directory containing this xiki action
        if ! Notes.in_home_xiki_dir?(file)
          Grab.prepend_cd_if_changed command, :dir=>File.dirname(file)   #> |||
        end

        DiffLog.quit_and_run command #, :dir=>Shell.dir   #> |||

        return # Xiki["& #{command}"]

      elsif txt =~ /\A.+\n  \|( |$)/

        # foo > (indent) > | bar, so expand, pass indented as args...

        path = Xik.path txt, 2

        args = [path[1]]
        args += options[:items] if options[:items].any?
        command = path[0]
        command = Path.escape(command) if command =~ /\A[$%&] /

        result = Xiki.expand command, args   #> |||

        # When to unescape?
        Tree.unquote!(result) if result && path[0] == "ruby"

        return result

      elsif txt =~ /\A\n*\$ /

        # "$ foo", so run shell command...

        # Remove all lines but first
        command = txt[/\A\n*(\$ .+)/, 1]
        command = Path.escape command

        options_in = {}
        options_in[:args] = options[:items] if options[:items].any?

        # Options.propagate_important_options(options) do |options|
        output = Xiki[command, options_in]   #> |||||

        output = "<*" if output == "|"
        return output

      elsif txt =~ /\A\+ [a-z0-9]/i

        # + bar items, so treat as a tree...

        # !... lines after tree, so chop them...
        code = txt.slice!(/\n\n! .*/m)

        xik = Xik.new txt
        path = Path.join(options[:items]) || ""
        path = Path.split path

        path = Path.join path   #> ""
        result = xik[path, :eval=>options]

        # "! code" and no result from the tree, run it

        if ! result && code
          code = code.strip.gsub(/^! ?/, '')

          options[:args] = options[:items]
          result = Code.eval code, file, options[:heading_line_number]+1, {:pretty_exception=>1, :simple=>1}, options   #> |||||||||
          return result
        end

        return result

      elsif txt =~ /\A.+\n  / && txt !~ /\A[{]/
        # Square brackets probably mean Json

        # foo > (indent) > bar, so use children as argument or present as menu items...

        just_one_child = txt =~ /\A.+\n  (.+)\n*\z/

        # Just pass as arg if single child...
        just_one_child = Line.without_label(:line=>$1) if just_one_child

        # /, so show child items
        if options[:items] == [] && ! just_one_child
          return txt.sub(/.+\n/, '')   # Return all but first line
        end

        command = txt[/.+/]   # First line

        # /child, so pass it to the parent
        # Delete all options except for important ones
        options.keys.each do |key|
          options.delete key if ! [:items, :task, :path].member? key
        end

        items = just_one_child ? [just_one_child] : options[:items]

        return Xiki["#{command}", items, options]   #> ||||||||
      end

      # Action contents are "command/  content", so pass content to command...

      txt = txt.strip

      # Just one line (that isn't blank), so xiki eval it...
      if txt.any? && txt.scan(/\n/).size == 0

        # "<= foo", so replace parent
        if txt =~ /^<= /
          # Todo > This may cause problems if line is already root > will go up to root one too many times
          Command.launch_after_collapse_root :line=>txt

          View.remove_last_undo_boundary
          return ""
        end

        # Delete all options except for important ones
        options.keys.each do |key|
          options.delete key if ! [:items, :task, :path].member? key
        end

        options[:go] = 1 if txt =~ /\/$/   #> "ayeeee: {:items=>[], :heading_line_number=>14, :heading_found=>\"> .aye\", :args=>[]}"
        result = Xiki.expand txt, options[:items], options   #> "ayeeee: []"

        return result
      end

      # We didn't recognize action's content, so just show literal output...

      txt

    end

    def self.tasks_dropdown options
      task = options[:task]

      xik = Xik.new %`
        * add note
          ! Notes.add_note_task options
        * web search
          ! name = Topic.remove_topic_syntax options[:name]
          ! name = TextUtil.whitespace_case name
          ! Xiki::Google.search "\#{name}", :via_os=>1
        * navigate
          ! structure = TopicExpander.local_structure options[:name]
          ! file = structure.values[-1][1]
          ! View.open file
      `

      task[0].sub!(/^/, '~ ') if task && task[0]
      xik[task, :eval=>options]

    end

    def self.found_by_user structure, heading, options={}

      TopicExpander.search_local_structure(structure)   # Populate hash of matching tasks

      # Narrow down to only ones that contain the heading...

      by_user, shared_txt, shared_txt_file = {}, nil, nil

      if heading =~ /^> /
        heading_words = heading[/..(.+)/, 1]

        structure.each do |k, v|

          v[2].each do |k, txt|

            # "> Heading", so look for it...

            if k =~ /\A> (@\w* )?#{Regexp.quote heading_words}$/
              user = $1
              user = "@private" if ! user
              if user == "@ "
                shared_txt, shared_txt_file = txt, v[1]
                user = "@shared"
              end
              user = user.strip

              by_user[user] = v[1]
            end
          end
        end

      else

        # "+ path", so look for it...

        structure.each do |k, v|
          v[2].each do |k, txt|

            # "path", so look for "> .Foo" heading, by extracting and comparing pathified version of each

            k = Notes.heading_to_path k#, :remove_username=>1
            if k =~ /\A(@\w* )?#{Regexp.quote heading}$/
              user = $1
              user = "@private" if ! user
              if user == "@ "
                shared_txt, shared_txt_file = txt, v[1]
                user = "@shared"
              end
              user = user.strip

              by_user[user] = v[1]
            end

          end
        end
      end

      # Parse shared task > Extract upvoted usernames > Remove @shared if appropriate...

      if shared_txt
        label_regex = XikihubClient.comment_or_upvote_label_regex "\\w+"

        show_shared = false   # Assume all lines are redundant upvotes until proven otherwise
        shared_txt.strip.gsub(/\n\n+/, "\n").split("\n").each do |line|

          upvote = line =~ /^upvote/i && line =~ /\A\n*#{label_regex}\n*\z/
          # Found non-upvote, so shared should be shown
          break show_shared = true if ! upvote

          username = line[/@\w*/]   #> @boo

          # Upvoted user is already installed, so no need to add it or do anything else
          break if by_user[username]

          by_user[username] = [:shared, shared_txt_file]

        end

        # Remove shared if we shouldn't show it
        # Removed shared if > only upvotes that are also installed
        by_user.delete("@shared") if ! show_shared

      end

      by_user
    end

    # .Test username_from_heading
    # ! TopicExpander.username_from_heading "> @steve Hey"
    # ! TopicExpander.username_from_heading "> @ Hey"
    # ! TopicExpander.username_from_heading "> Hey"
    def self.username_from_heading heading #, options={}
      username = heading[/> (@\w*) /, 1]
      return "@private" if username == nil
      return "@shared" if username == "@"
      username
    end

    # TopicExpander.delete_username_from_heading "> Heading"
    # TopicExpander.delete_username_from_heading "> @ Heading"
    # TopicExpander.delete_username_from_heading "> @user Heading"
    def self.delete_username_from_heading heading #, options={}
      heading.gsub! /^> @\w* /, "> "
      heading
    end


  end
end

