module Xiki
  class Topic

    @@cache = {}

    def self.topicize_selection options={}

      View.deselect

      txt = View.delete *View.range

      txt = Tree.pipe(txt).gsub(/^/, "    ")

      txt = "Topic here\n  > Heading here\n#{txt}    |\n    + save\n    + share\n"
      View.<< txt, :dont_move=>1
      View << ":" if options[:search]
      View.remove_last_undo_boundary

      cursor = View.cursor
      Overlay.face :diff_green, :left=>View.cursor, :right=>Line.right
      Search.forward("Heading", :beginning=>1)
      Overlay.face :diff_green, :left=>View.cursor, :right=>Line.right
      View.cursor = cursor

      # Pause until they type any key
      key = Keys.press_any_key :message=>"Type any key..."
      return "" if key == [27]

      View.delete View.cursor, Line.right
      Search.forward("Heading", :beginning=>1)
      View.delete View.cursor, Line.right
      View.cursor = cursor

      View.<<(key[0].chr) if key.length == 1 && key[0] >= 32 && key[0] <= 126

    end


    # Mapped to Ctrl+Return.  Expands thing on current line.
    # Invokes Expander.expand.
    def self.xiki_key

      # Selection, so just quote for adding comment...

      line = Line.value

      # /:foo, so remove the dash and expand...

      if line =~ /^[ =]*:[a-z][a-z0-9 ]*$/i

        # Collapse first (if any children)
        Tree.collapse

        Line.sub! /:/, ""
      end

      path = Tree.path
      last = Path.split path[-1]

      # ^X on "foo/", so still show topics
      if line =~ /\A[a-z0-9][a-z0-9 ]*\/\z/i
        Line.sub! /\/$/, ''
        Launcher.launch
        return
      end

      # Already a topic, so just launch...

      return Launcher.launch if Topic.matches_topic_syntax?(path[-1])

      # Blank shell prompt ("$ "), so delete it and do ^W again...

      if path[-1] =~ /\A\$ ?\z/
        line = Line.value

        if line =~ /^ /
          line.sub! /^( +)(.*)/, "\\1= xiki/"   # Indent, so add "=" after indent
        else
          line = "xiki/"   # No indent, so delete whole line
        end

        Line.sub! /.*/, line
        return Launcher.launch
      end

      # ^T on "% foo", so change do "$ foo"...

      if path[-1] =~ /\A\% /
        Line.sub! /^\% /, "$ "
        return self.xiki_key
      end

      # $ foo, so expand as topic...

      if path[-1] =~ /\A\$ /
        return Launcher.launch :ctrlx=>1
      end

      # Blank line, so insert "xiki/" to show recent...

      if path == [""]
        View << "xiki/"
        Launcher.launch
        return
      end

      # topic/> Heading, so navigate to heading

      if last.length == 2 && Topic.matches_topic_syntax?(last[0])

        if last.length >= 2
          is_topic_action = Topic.matches_topic_syntax?(last[0]) && Topic.matches_topic_syntax?(last[1])
          is_topic_heading = Topic.matches_topic_syntax?(last[0]) && last[1] =~ /^> /
        end

        # topic/action, so turn back into heading...

        # if is_topic_action
        #   return Launcher.launch :go=>1
        # end
        if is_topic_action
          (last.length-2).times{ Tree.to_parent }
          Tree.collapse if Tree.children?
          Launcher.launch((last.length > 2 ? :change_to_heading_and_launch : :change_to_heading)=>1)
          return ""
        end

        # ^X on "+ action" under topic (and maybe @foo), so navigate...

        # "topic/> Heading", so toggle between action and note

        if is_topic_heading

          (last.length-2).times{ Tree.to_parent }
          Tree.collapse if Tree.children?

          # Launch if was expanded, leave collapsed if was collapsed
          Launcher.launch(:task=>[last.length > 2 ? "as action" : "make action"])
          return ""

        end

      end

      # command/, so treat it like a topic...

      # topic/arg, so just do launch

      if last.length > 1

        # ^T on "topic/.../| Foo", so unquote it with equals...

        if last[-1] =~ /\n/

          line = View.line

          # Grab quoted text > contiguous lines
          # Tree.sibling_bounds(:must_match=>"\\|")
          bounds = Tree.sibling_bounds(:must_match=>"\\|")

          was_at_top = Line.left == bounds[0]

          txt = View.delete bounds[0], bounds[-1]

          # unquote and put back > with =

          indent = Line.indent txt
          txt.gsub!(/^( +)\|/, "\\1 ")
          txt.gsub!(/^ +$/, "")
          txt.gsub!(/\n\n\n+/, "\n\n")
          txt = "#{indent}=\n#{txt}"

          View << txt
          View.remove_last_undo_boundary

          # 2nd line is indented quote
          if  was_at_top && txt.unindent =~ /^=\n  \w.*\n    \|/
            line += 1
          end

          View.line = line+1

          Line.to_words

          return ""
        end

        if last[0] == "markers" # && last.length > 1
          # Ol "continue here > continue with > '~ navigate'!!"
          return Launcher.launch :task=>["navigate"]
        end

        return Launcher.launch :ctrlx=>1
      end

      if last[0] =~ /\A[a-z0-9 ]+\z/i
        path[-1].sub!(/^/, "-")   # Make it look like a topic
        return Tree.<< Expander.expand path
      end

      # Something else, so say we can't do it...

      Tree.<< "| Move the cursor to some words\n| or a shell command first."

    end

    # Topic.matches_topic_syntax? "item"
    #   nil
    # Topic.matches_topic_syntax? ":item"
    #   0
    # Topic.matches_topic_syntax? ":item/"
    #   0
    def self.matches_topic_syntax? item
      item =~ /\A[a-z0-9][a-z0-9_ ]*\/?\z/i
    end

    def self.matches_topic_word? item
      item =~ /\A[a-z][a-z0-9_ ]*\z/i
    end

    # Topic.remove_topic_syntax "hey-"
    #   hey
    # Topic.remove_topic_syntax "hey"
    #   hey
    def self.remove_topic_syntax item
      item.sub(/\A-/, '')
    end

    def self.favorite topic, options

      topic = self.remove_topic_syntax topic

      # Make dir if not there
      dir = File.expand_path "~/.xiki/misc/favorite/"
      FileUtils.mkdir_p dir   # Make sure it exists
      file = "#{dir}/topics.xiki"
      txt = File.read(file) rescue ""
      txt = txt.split "\n"

      txt.unshift topic
      txt.uniq!
      txt = txt.map{|o| "#{o}\n"}.join

      File.open(file, "w") { |f| f << txt }
      options[:no_search], options[:line_found] = 1, 3
      "
      | Added favorite.
      | Next time you do list+favorites (^L ^F) this will appear.
      = ok/
      "
    end

    # Topic.all
    def self.list
      # Read in files and mod dates
      files = []
      files += Dir["#{Bookmarks['~/xiki/']}*"]

      # For now > don't include commands > since Ctrl+T on them does nothing useful
      # Commands (user and main)

      files = files.sort_by{ |f| File.mtime(f) }.reverse

      # Remove paths
      files.map!{|o| File.basename(o, ".*").gsub('_', ' ')}
      files.uniq
    end

    def self.tab_key options={}

      FileTree.extra_line_if_end_of_file

      line = Line.value

      # foo..., so remove dots from search string
      txt = line.sub /\.\.\.\z/, ''

      topics = self.list
      topics = topics.grep(/^#{txt}/)
      topics = topics.map{|o| "#{o}\n"}.join('')

      if ! topics.any?
        View.flash("- no completions found!") if options[:flashing_ok]
        return nil
      end

      # Add dots if not there yet...

      if line !~ /\.\.\.\z/
        Line.sub! /$/, "..."
      end

      # TopicExpander.reposition_top_topics line, topics#, top_topics

      Tree.<< topics# if topics.any?

      topics   # Some callers want a return value, to know if we found anything
    end

    def self.pin_to_top topic, item

      dir = File.expand_path "~/.xiki/misc/topic_top"
      FileUtils.mkdir_p dir   # Make sure it exists

      # Open tasks/topic, if any...
      file = "#{dir}/#{topic}.xiki"
      txt = File.read(file) rescue ""
      txt = "#{item}\n#{txt}"
      txt = txt.split("\n").uniq.join("\n")+"\n"
      File.open(file, "w") { |f| f << txt }

      "<* - Will appear at top next time"
    end


    def self.shell_command_to_topic shell

      # $ foo -> foo
      # $ foo bar -> foo bar
      # $ foo -b -> foo b (accept a dash arg, but remove the dash)
      # $ foo bar.txt -> foo bar (because it has a dot)
      # $ foo bar bah -> foo bar (limit to only one arg)

      # Just prepend with colon

      shell = shell.sub(/^[$%&] /, '')
      topic = shell.split(" ")
      # Only consider command and first argument
      topic = topic[0..1]
      # Cut off arg if it's not all letters
      topic.pop if topic[-1] !~ /^[a-z][a-z0-9]*$/i   # ["fook"]
      topic = topic.join " "   # "fook"

    end


    def self.append_log line

      return if XIKI_SERVER_MODE

      # More than one linebreak, so don't add to log (one linebreak is expected?)
      return if line =~ /\n/

      # Don't let "history" show up under "history"
      return if [
        "save", "save without diff", "history", "unsaved",
        "content", "window", "list",
        "hop", "do", "run",
        "as", "enter", "yours",
      ].member?(line.strip)

      file = File.expand_path("~/.xiki/misc/logs/topic_log.xiki")
      FileUtils.mkdir_p File.dirname(file)   # Create dir if it doesn't exist
      File.open(file, "a") { |f| f << "#{line}\n" }
    end


    # > .Test topic_to_filename
    # ! Topic.topic_to_filename "two words"
    def self.topic_to_filename topic
      File.expand_path("~/xiki/#{topic.gsub(/[ -]/, '_')}.xiki")
    end


    # > .Test extracting ":lines"
    # xiki api/extract wrapper patterns


    def self.extract_opener_patterns txt

      hash = {}

      heading, command = nil, nil

      # No args, so only look for "|:anything"
      txt.split("\n").each do |line|
        if line =~ /^> /
          command = nil
          next heading = line
        end

        # Any line that's not indented can be considered a command
        next command = line if line =~ /^[^ \n|]/

        # Don't look for patterns unless heading is action
        next if heading !~ /^> (@\w* )?\./

        if pattern = line[/^  \|(:.+)/, 1]
          next if ! command
          key = "#{command}/#{pattern}"
          next if hash[key]   # Only use the first one
          next hash[key] = heading
        end

        if pattern = line[/^\|(:.+)/, 1]
          key = "#{pattern}"
          next if hash[key]   # Only use the first one
          next hash[key] = heading
        end

      end

      hash
    end

    def self.init_cache

      return if XIKI_SERVER_MODE

      # Load xsh.xiki file into a hash in memory
      # Maybe more in the future

      txt = File.read(File.expand_path("~/xiki/xsh.xiki")) rescue nil
      return if ! txt
      hash = Notes.wiki_headings_to_hash(txt)

      self.cache["xsh"] = hash
    end


    def self.cache
      @@cache
    end


  end
end
