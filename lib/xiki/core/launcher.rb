require 'xiki/core/effects'
require 'xiki/core/requirer'
require 'xiki'

module Xiki
  class Launcher

    CLEAR_CONSOLES = [
      "ol",
    ]

    @@log = File.expand_path("~/.xiki/misc/logs/xiki_commands_log.xiki")

    # Use @launcher/options/show or launch/ to enable.

    @@just_show = false

    @@launchers ||= {}
    @@launchers_procs ||= []
    @@launchers_parens ||= {}
    @@menus ||= [{}, {}]   # [.menu files, .rb files]

    def self.menus
      @@menus
    end

    # Deprecated!
    def self.menu_keys
      (@@menus[0].keys + @@menus[1].keys).sort.uniq #.select do |possibility|
    end

    def self.menu
      %`
      - .setup/
        > Toggle Temporarily just showing the launcher that matched
        - .show or launch/
      - .registered/
        - menu files/
        - menu classes/
      - docs/
        > Summary
        | Launcher is the class that handles "launching" things (double-clicking
        | on a line, or typing Ctrl+X).
      - api/
        > Open menu in new buffer
        = Launcher.open "computer"

        > Insert monu
        @ Launcher.insert "computer"   # Assumes you're on a blank line

        > Invoke (used behind the scenes?)
        @ p Launcher.invoke 'Computer', 'computer/ip/'
      `
    end

    def self.registered kind=nil

      if kind == "menu files"
        keys = Launcher.menus[0].keys
        return keys.map{|o| "@#{o}/"}.join("\n")
      elsif kind == "menu classes"
        keys = Launcher.menus[1].keys
        return keys.map{|o| "@#{o}/"}.join("\n")
      end

      raise "- .registered doesn't know #{kind}!"
    end


    def self.show_or_launch
      @@just_show = ! @@just_show
      View.flash "- Will #{@@just_show ? 'just show' : 'actually launch'}!"
    end

    def self.last path=nil, options={}
      path = path.sub /^last\/?/, '' if path

      paths = IO.readlines self.log_file

      # /..., so list all roots

      if path.blank?
        paths.map!{|o| o.sub /\/.+/, '/'}   # Cut off after path
        paths = paths.reverse.uniq
        paths.delete "- #{options[:omit]}/\n" if options[:omit]
        return paths.join
      end

      # Root passed, so show all matches

      paths = paths.select{|o| o =~ /^- #{Notes::MARKER_REGEX}#{path}./}

      bullet = options[:quoted] ? "|" : "-"

      if options[:exclude_path]
        paths.each{|o| o.sub! /^- (#{Notes::MARKER_REGEX})#{path}\//, "#{bullet} \\1"}
        paths = paths.select{|o| o != "#{bullet} "}
      else
        paths = paths.map{|o| o.sub /^- #{Notes::MARKER_REGEX}/, '@'}
      end
      paths = paths.reverse.uniq
      paths.delete_if{|o| o == "| \n"}
      paths.join
    end

    # Called by menu to show log contents, with newest first.
    def self.log options={}

      lines = IO.readlines self.log_file

      # foo/=log, so narrow down to just this menu...

      trunk = Tree.path
      if trunk.length > 1 && trunk[-2] != "menu/history"   # Show all if under this menu
        lines = lines.select {|o| o.start_with? "- #{trunk[-2]}"}
      end

      # Remove indented lines (shouldn't have been logged with unescaped linebreaks)...

      bullets = options[:arrows] ? "<< " : "="

      lines = lines.reverse.uniq.map{|o| o.sub /^/, bullets}
      lines = lines.select{|o| o =~ /^[^ ]/}   # Only grab unindented lines (in case there's weirdness in the log)

      lines.join
    end

    def self.log_file
      @@log
    end

    def self.add *args, &block
      arg = args.shift

      raise "Launcher.add called with no args and no block" if args == [nil] && block.nil?

      if arg.is_a? Regexp   # If regex, add
        @@launchers[arg] = block
      elsif arg.is_a? Proc   # If proc, add to procs
        @@launchers_procs << [arg, block]
      elsif arg.is_a?(String)
        self.add_menu arg, args[0], block
      else
        raise "Don't know how to launch this"
      end
    end

    def self.add_menu root, hash, block
      if hash.nil? && block   # If just block, just define
        return @@menus[1][root] = block
      elsif hash.is_a?(Hash) && hash[:menu_file] && block
        return @@menus[0][root] = block
      end

      # If just root, we'll use use class with that name
      if block.nil? && (hash.nil? || hash[:class])
        clazz = hash ? hash[:class] : root
        clazz.sub!(/(\w+)/) {TextUtil.snake_case $1} if hash
        self.add root do |path|
          # Make class me camel case, and change Launcher.invoke to Menu.call
          Launcher.invoke clazz, path
        end
        return
      end

      menu = hash[:menu]
      if menu
        if menu =~ /\A\/.+\.\w+\z/   # If it's a file (1 line and has extension)
          require_menu menu
          return
        elsif menu =~ /\A[\w \/-]+\z/   # If it's a menu to delegate to
          self.add root do |path|
            Command.call menu, Tree.rootless(path)
          end
          return
        end

        self.add root do |path|   # If text of the actual menu
          # Different from Menu[...] or .drill?
          Tree.children menu, Tree.rootless(path)
        end
        return
      end

      raise "Don't know how to deal with: #{root}, #{hash}, #{block}"
    end


    # Collapses and re-runs line in current view, or in :t (sometimes jumping to the nth labeled line).
    def self.do_last_launch options={}

      prefix = options[:prefix] || Keys.prefix(:clear=>true)

      View.kill if View.name == "tasks markers/"

      # Clear out time of last log, so it always shows heading
      Ol.clear_pause

      orig = View.index
      orig_file = View.file

      filter_to_current_file = prefix == :u

      Ol.filter = orig_file if filter_to_current_file

      # If ol buffer open, grab line number so we can restore it after
      ol_cursor_position = nil

      if View.buffer_open? "ol"
        View.to_buffer "ol"
        ol_cursor_position = Line.number if ! View.at_bottom
      end

      if ! options[:dont_launch]
        CLEAR_CONSOLES.each do |buffer|
          View.clear buffer
        end
      end

      if options[:here] # || prefix ==:uu
        View.to_nth orig
      else

        View.open(options[:bookmark] || "%n", :stay_in_bar=>1)
        if prefix.is_a?(Fixnum) && prefix > 0
          View.line = prefix
        end
      end

      # If do+1 or do+2 etc, move to nth thing to run...

      if options[:nth] || options[:label]
        # Remember where to go back to, if we were in $todo
        if orig_file == Bookmarks["%n"]
          line, column = View.line, View.column
        end

        # Always do highest
        View.to_highest

        if options[:nth]
          options[:nth].times{ Notes.next_marker }
        elsif options[:label]

          # Find "- foo) bar", "- ) foo", or "- )\nbar"
          Notes.jump_to_label options[:label]

          Notes.next_line_when_marker
        end

        return if options[:go]

        # Do nothing if went to the end without finding anything
        if View.cursor == View.bottom
          View.to_highest
          View.line, View.column = line, column if line
          return View.flash "- No lines are marked.  Use layout+mark."
        end

      end


      if prefix == 0
        Move.backward
        Line.to_beginning
        return
      end

      line_value = Line.value


      # Better plan > always collapse (only does something if something under it)...

      Tree.to_parent if Line =~ /^ +- backtrace:$/   # If we went to "- backtrace:", go up again

      if options[:dont_launch]
        Line.to_beginning
        return
      end


      # Collapse if children of this line
      Launcher.launch_or_collapse :just_collapse=>1

      Effects.blink

      if (options[:here] && ! options[:nth]) || prefix == :-   # :here is only used by do+up
        Launcher.launch
      else
        Launcher.launch(:no_search=>true)   #> |
      end

      return if prefix == :-

      Ol.filter = nil if filter_to_current_file

      if ol_cursor_position
        View.to_buffer "ol"
        View.line = ol_cursor_position
      end

      View.line, View.column = line, column if line

      # Don't go to orig if :n > and we already went to a file...

      return if options[:bookmark] == "%links" && View.file != Bookmarks["%links"]

      View.to_nth orig

      # do+expand, so reflect value in comment, if line is "Ol foo" or "ol[foo]"
      View.layout_outlog(:prefix=>:-) if prefix == :u && Line =~ /^ *Ol( |!|\.>>)/
    end

    def self.enter_last_launched
      Launcher.insert self.last_launched_menu
    end

    def self.last_launched_menu
      bm = Keys.input(:timed => true, :prompt => "bookmark to show launches for (* for all): ")

      menu =
        if bm == "8" || bm == " "
          "nav history/"
        elsif bm == "."
          "nav history/#{View.file}/"
        elsif bm == "3"   # Means show only searches
          # Make it not do weird search behavior where it jumps down
          "nav history/#/"
        elsif bm == ";" || bm == ":" || bm == "-"   # What does this mean?
          "nav history/:/"
        else
          "nav history/:#{bm}/"
        end
    end

    # Superceded by Invoker.invoke after Unified refactor

    # Invokes the .menu method in a class
    # Should this be in another class?  Maybe menu.rb - maybe invoker.rb?
    def self.invoke clazz, path, options={}

      default_method = "menu"
      # If dot, extract it as method
      if clazz =~ /\./
        clazz, default_method = clazz.match(/(.+)\.(.+)/)[1..2]
      end

      if clazz.is_a? String
        # Require it to be camel case (because .invoke will be Menu.call "Class"
        # if lower case, will assume Menu.call "path"
        camel = TextUtil.camel_case clazz
        clazz = $el.instance_eval(camel, __FILE__, __LINE__) rescue nil

      elsif clazz.is_a? Class
        camel = clazz.to_s
      end

      snake = TextUtil.snake_case camel

      raise "No class '#{clazz || camel}' found in launcher" if clazz.nil?

Ol["oh, this path is an array: #{path}!"] if path.is_a?(Array)
      Ol["Changed > might cause problems?!"]
      args = path.is_a?(Array) ?
        path : Path.split(path, :return_path=>1)
      #         path : Menu.split(path, :return_path=>1)

      # Call .menu_before if there...

      method = clazz.method("menu_before") rescue nil

      if method
        code = "#{camel}.menu_before *#{args.inspect}"
        returned, out, exception = Code.eval code

        return CodeTree.draw_exception exception, code if exception
        if returned

          returned = returned.unindent if returned =~ /\A[ \n]/
          return returned
        end
      end

      menu_arity = nil
      txt = options[:tree]

      # Call .menu with no args to get child menus or route to other method...
      if txt.nil?
        method = clazz.method(default_method) rescue nil
        if method && method.arity == 0
          menu_arity = 0
          code = "#{camel}.#{default_method}"

          returned, out, exception = Code.eval code
          return CodeTree.draw_exception exception, code if exception
          txt = CodeTree.returned_to_s returned   # Convert from array into string, etc.
        end
      end

      # Error if no menu method or file
      if method.nil? && txt.nil? && ! args.find{|o| o =~ /^\./}
        cmethods = clazz.methods - Class.methods
        return cmethods.sort.map{|o| ".#{o}/"}
      end


      # If got routable menu text, use it to route (get children or dotify)...
      if txt
        txt = txt.unindent if txt =~ /\A[ \n]/
        raise "#{code} returned nil, but is supposed to return something when it takes no arguments" if txt.nil?

        tree = txt

        txt = Tree.children tree, args

        if txt && txt != "- */\n"
          # Pass in output of menu as
            # 1st parameter: .menu_after output, *args
          return self.invoke_menu_after clazz, txt, args
        end

        # Copy dots onto args, so last dotted one will be used as action

        Tree.dotify! tree, args

        # TODO: when to invoke this?
          # Maybe invoke even if there was no .menu method
            # Is that happening now?

        # If .menu_hidden exists, dotify based on its output as well...
        method = clazz.method("menu_hidden") rescue nil
        if method

          returned, out, exception = Code.eval "#{camel}.menu_hidden"
          if returned && returned.is_a?(String)
            returned = returned.unindent
            Tree.dotify! returned, args
          end
        end

      end
      # Else, continue on to run it based on routified path

      # TODO: Maybe extract this out into .dotified_to_ruby ?

      # Figure out which ones are actions

      # Last .dotted one is the action, and non-dotted are variables to pass
      actions, variables = args.partition{|o| o =~ /^\./ }
      action = actions.last || ".#{default_method}"
      action.gsub! /[ -]/, '_'
      action.gsub! /[^\w.]/, ''


      # Call .menu_after if appropriate...

      if action == ".menu" && txt.nil? && menu_arity == 0
        return self.invoke_menu_after clazz, txt, args
      end


      # .menu has args, so invoke it with them...

      args_orig = args
      args = variables.map{|o| "\"#{CodeTree.escape o}\""}.join(", ")

      # TODO: use adapter here, so we can call .js file?

      # TODO .menu_after: Check for arity - if mismatch, don't call, but go straight to .menu_after!
      # We could probably not worry about this for now?

      code = "#{camel}#{action.downcase} #{args}".strip

      txt, out, exception = Code.eval code

      txt_orig = txt
      txt = CodeTree.returned_to_s(txt)   # Convert from array into string, etc.

      txt = txt.unindent if txt =~ /\A[ \n]/

      return CodeTree.draw_exception exception, code if exception

      txt = self.invoke_menu_after clazz, txt_orig, args_orig

      txt
    end

    def self.invoke_menu_after clazz, txt, args
      camel = clazz.to_s
      method = clazz.method("menu_after") rescue nil
      return txt if method.nil?

      code = "#{camel}.menu_after #{txt.inspect}, *#{args.inspect}"
      returned, out, exception = Code.eval code

      return CodeTree.draw_exception exception, code if exception
      if returned

        returned = returned.unindent if returned =~ /\A[ \n]/
        return returned
      end

      txt   # Otherwise, just return output!"

    end

    def self.add_class_launchers classes

      classes.each do |clazz|
        next if clazz =~ /\//

        # Why is this line causing an error??
        #       clazz = $el.el4r_ruby_eval(TextUtil.camel_case clazz) rescue nil
        #       method = clazz.method(:menu) rescue nil
        #       next if method.nil?

        self.add clazz do |path|
          Launcher.invoke clazz, path
        end
      end
    end

    def self.append_log path
      return if View.name =~ /_log.xiki$/

      path = path.sub /^[+-] /, ''   # Remove bullet
      path = "#{path}/" if path !~ /\//   # Append slash if just root without path

      return if path =~ /^(h|log|last)\//

      File.open(@@log, "a") { |f| f << "#{path}\n" } rescue nil
    end

    # Insert menu right here and launch it
    #
    # Launcher.insert "computer"
    def self.insert txt, options={}
      View.insert txt
      $el.open_line(1)

      Launcher.launch options
    end

    def self.show menu, options={}
      self.open menu, options.merge(:no_launch=>1)
    end

    # Open new buffer and launch the menu in it
    #
    # Launcher.open "ip"
    def self.open menu, options={}

      return self.insert(menu, options) if options[:inline]

      View.to_after_bar if View.in_bar? && !options[:bar_is_fine]

      dir = options[:buffer_dir] || View.dir

      buffer = options[:buffer_name]

      if ! buffer
        # For buffer name, handle multi-line strings
        buffer = menu.sub(/.+\n[ -]*/m, '').gsub(/[.,]/, '')
        buffer = buffer.sub(/^[+-] /, '')
        buffer = buffer.sub(/^= ?/, '')
      end

      View.to_buffer buffer, :dir=>dir

      View.clear
      Notes.mode
      View.wrap :off

      txt = menu

      if txt.blank?
        return View.insert("\n", :dont_move=>1)
      end

      dir = options[:dir] and txt = "- #{dir.sub /\/$/, ''}/\n  - #{txt}"

      View << "#{txt.strip}"

      $el.open_line 1

      if options[:choices]
        View.to_highest
        Tree.filter
        return
      end

      if options[:no_launch]
        # Go to 2nd line (or first, if only one line (and linebreak))
        View.line = View.number_of_lines > 2 ? 2 : 1
        return
      end

      options.merge! :launcher_open=>1

      Launcher.launch options   #> |||||||
    end


    #
    # Adds a menu (command) for each file in tools?
    #
    def self.load_tools_dir

      dir = "#{Xiki.dir}lib/xiki/tools"
      Files.in_dir(dir).each do |f|

        next if f !~ /^[a-z].*\..*[a-z]$/ || f =~ /__/
        next if f =~ /\.menu$/

        path = "#{dir}/#{f}"
        require path
      end

      "- reloaded!"
    end

    #
    # Launches "menu/item", first prompting for name.  Used by search+like_menu
    # and other places.
    #
    # If matches substring, shows the possible matches and does an isearch.
    #
    # Menu.like_menu "htm"
    #
    def self.like_menu item, options={}
      return if item.nil?

      menu = Keys.input :timed=>true, :prompt=>"Pass '#{item}' to which command? (or space if it's the command): "

      return self.open(item, options) if menu == " "   # Space means text is the menu

      matches = Xiki::Command.completions menu

      if matches.length == 1
        return self.open("#{matches[0]}/#{item}", options)
      end

      self.open(matches.map{|o| "#{o}/#{item}\n"}.join(''), options.merge(:choices=>1))
      right = View.cursor
      Move.to_previous_paragraph

      Tree.filter :left=>View.cursor, :right=>right
    end

    def self.search_like_menu
      txt = Search.stop
      self.like_menu txt
    end

    def self.as_update
      Keys.prefix = "update"
      Launcher.launch :leave_bullet=>1
    end

    def self.as_delete
      Keys.prefix = "delete"
      Launcher.launch
    end

    def self.as_open
      Keys.prefix = "open"
      Launcher.launch
    end

    def self.enter_all
      return FileTree.enter_lines(/.*/) if Line.blank?

      Keys.prefix = "all"
      Launcher.launch
    end

    # Shortcut for passing "outline" prefix and launching.
    def self.enter_outline

      # Blank line, so insert file from bookmark first...

      if Line.blank?

        message = "File to insert outline for: "
        View.flash message
        path = Keys.input "#{message}: ", :timed=>1

        path = Bookmarks["%#{path}"] if path =~/^\w/   # They typed a word, so expand it as a bookmark

        View.insert path

      end

      # If there's a numeric prefix, add it

      Launcher.launch :task=>["outline"]
    end


    def self.web_menu line
      Line << "/" unless Line =~ /\/$/
      url = "http://#{line}"
      url.sub! /\.\w+/, "\\0/xiki"
      url.gsub! ' ', '+'

      begin
        response = HTTParty.get(url)
        Tree << response.body
      rescue Exception=>e
        Tree << "- couldn't connect!"
      end
    end

    # def self.expand options={}
    def self.launch_or_collapse options={}

      return Grab.quote_selection if View.selection?

      line = Line.value

      Ol.clear_pause

      # If no prefixes and children exist, collapse

      if ! Line.blank? && View.name != "ol" && ! options[:go]


        # There's a child right under this line, so just collapse it
        if Tree.children?
          Tree.minus_to_plus
          Tree.collapse
          return

        # There's a child at the end of the paragraph, so collapse it
        elsif line =~ /^ *\|/
          # Or, collapse children at end, if any
          indent = Line.indent line

          ignore, right = View.paragraph(:bounds=>true)
          orig = View.cursor
          View.cursor = right-1
          if Line =~ /^#{indent} /
            Tree.to_parent
            Tree.collapse
            View.cursor = orig
            return
          end

          # Go back to where we were, And do nothing
          View.cursor = orig

        end

      end


      return if options[:just_collapse]


      self.launch options.select{|key, value| [:go].include?(key)}   #> ||
    end


    # Invoked by C-x, via Launcher.go.
    # Gets info from view, delegates to Expander.expand, then inserts result.
    def self.launch insert_options={}

      line, line_number = Line.value, Line.number

      Line.<<("\n", :dont_move=>1) if Line.right == View.bottom   # If at end of view add linebreak, in none.

      options = insert_options.merge(:client=>"editor/emacs")

      # Treat -foo as same as foo (and change it)

      if line =~ /^-[a-z]/i
        # This may be too simple > maybe delegate to a method that does stuff specific to the commands?
        # and/or > make the single letter equivalent have more logic!
        Line.sub!(/^-/, '')   # Change variable and the line itself
        line.sub!(/^-/, '')
      end

      begin
        path = Tree.path
      rescue Exception=>e
        # Maybe make this be WellformedTreeException
        if e.is_a?(RuntimeError) && e.message =~ /well-formed tree/
          path = ""
          options[:not_well_formed] = 1
        end
      end

      path_last = Path.split(path[-1])

      # Commit: swap colon and pipe quotes

      if prefix = Keys.prefix; options[:prefix] = prefix; end

      # In case launching a "~ task" item, delete the siblings (the'll be added back later)...

      option_items_orig = self.delete_option_item_siblings path   #> |||

      # "foo/" line (and it's the parent), so pretend like they did ^G on it
      if path_last.length == 1 && Line =~ /^ *([=+-] )?[a-z][a-z0-9 ]*\/$/i # || line =~ /^ += ?[a-z][a-z ]*\/$/i
        options[:go] = 1
      end

      # Maybe nest these within new :limits option (and delete before
      # passing to menu) to avoid too many options passed into menus.
      # If this change is made, be sure to make pattern launchers look
      # in the new place:
      # /projects/xiki/lib/xiki/core/
      #   - pattern.rb
      #     |           if nested = value[options[key]]

      if view = View.name; options[:target_view] = view; end
      if file = View.file; options[:target_extension] = File.extname(file); end

      # Set :dir, based on path ancestors, or else Shell.dir
      # Ol "dir", dir
      dir = Tree.closest_dir(path[0..-2]) if path.is_a?(Array)
      options[:dir] = dir || Shell.dir

      return if self.process_target_bullets_before line   # If the line has << and other arrow-ish bullets

      self.adjust_line_number_maybe path, options

      if options[:path_append]   # Append item to path (used for :task)
        path[-1] << "/" if path[-1] =~ /[^\/]$/   # Append slash if content that doesn't end in slash
        path[-1] << options[:path_append]
      end



      # Expand command...

      txt = Expander.expand path, options   #> ||||||||

      # File, so append slash

      # Ol "options", options   #> {:no_search=>true, :client=>"editor/emacs", :target_view=>"notes.xiki", :target_extension=>".xiki", :dir=>"/Users/craig/", :file_path=>"/tmp/d/", :expanders=>[Xiki::FileTree], :expanders_index=>1, :no_slash=>1, :output=>"  - d/\n    - d/\n      + d.txt\n    + a.txt\n    + c.xiki\n    + b.txt"}
      if options[:file_path] && ! options[:task] && txt !~ /\A~/ && ! options[:no_slash]
        insert_options[:add_slash] = 1
      end

      # Todo > possibly this several times, in case they ask for multiple?
        # (maybe max out at like 10 times)
      txt = self.expand_again_if_beg txt, options
      txt = txt.to_s if txt != nil && ! txt.is_a?(String)

      # Propagate certain options set by implementation, meant to control how it's inserted...

      Options.propagate_some_outward options, insert_options

      # Re-add task items and restore cursor if requested to put output under task...

      # :nest, so insert under??

      if options[:task] && options[:nest] && option_items_orig   #> nil
        # Keep using letters if under task
        insert_options[:hotkey] = 1 if ! options[:no_task]
        Line.next
        View << option_items_orig
        View.line = line_number
      end

      return if txt.blank?

      # Automatically repress slash if were on >... or |... line

      insert_options[:no_slash] = 1 if options[:args] && options[:args].last =~ /(^[>|:]|\n)/

      # "* foo" task item, so don't insert slashes after, and use hotkey search

      if txt =~ /\A\s*\* /
        # Suppress adding slash if doing a task on a quote
        insert_options[:no_slash] = 1 if options[:quote]
        # Suppress adding slash, unless on a file path (it will only add if it's actually a dir, which we want)
        insert_options[:no_slash] = 1 if ! options[:file_path]

        insert_options[:hotkey] = 1
      end

      # Todo > merge together with below?
        # First > try moving 3 lines below to after
      # Wait! > I get it > this is based in bullets in output
        # and > the below is based on bullets in the expanded line
      return if self.process_output_bullets txt, options, insert_options

      txt = txt.to_s if txt
      return if txt.blank?
      $el.deactivate_mark   # So aquamacs doesn't highlight something after double-click

      # Todo > merge together with above?
      self.process_target_bullets_after line, txt, insert_options   # Delete stuff if bullet was <~ or <+!


      # There was output, so change + to -

      Tree.plus_to_minus unless insert_options[:leave_bullet] || insert_options[:task]

      # Root item is just words (search), so don't append slash...
      options[:no_slash] = 1 if Topic.matches_topic_syntax? path[-1]

      # Snippet with 8 lines or less, so insert output under last "|..." line...

      if path_last[-1] =~ /\n.+\n/ && path_last[-1].scan("\n").length <= 8
        bounds = Tree.sibling_bounds(:must_match=>"\\|")
        View.cursor = bounds[3] - 1
      end

      Tree.<< txt, insert_options

      nil
    end

    def self.adjust_line_number_maybe path, options
      # It's a file path with numbers, so adjust the number first
        # What was this supposed to do?
    end

    def self.expand_again_if_beg txt, options

      return txt if ! txt.is_a? String

      return txt if txt !~ /\A=beg\/(.+)\/\z/   # Only try to do something if menu returned =beg/.../

      beg = $1

      return if ! options[:items] || ! options[:items][-1]   # Only try to do something if there's a path
      # It only makes sense to beg for siblings when there's a path (since we
      # wouldn't want to pass the siblings of the root menu as the path)

      options.delete :output   # Clear out, because handler won't over-write output

      # Can have siblings we can grab if it's |... or is just one item (unescaped)
      line = Line.without_label
      can_have_siblings = line =~ /^[|:]/ || Path.split(line).length == 1

      # Remember whether ends in slash and not quoted - means begged item should be appended to path, not replaced
      itemish = line !~ /^ *\|/  # && line =~ /\/$/

      if ! can_have_siblings   # If no siblings, just add \n, so they'll no we sent all the lines
        options[:items][-1] = "#{options[:items][-1]}\n"

      # Grab siblings and pass as last arg
      elsif beg == "quoted"   # Consecutive quoted siblings
        options[:items][-1] = Tree.siblings :string=>1
      elsif beg == "neighbors"   # Siblings, not crossing blank lines
        siblings = "#{Tree.siblings(:include_label=>1).join("\n")}\n"
        itemish ?
          options[:items].<<(siblings) :
          options[:items][-1] = siblings      # Includes current line
      elsif beg == "siblings"   # =beg/siblings/
        # Siblings, crossing blank lines
        siblings = "#{Tree.siblings(:cross_blank_lines=>1, :include_label=>1, :children=>1)}"
        itemish ?
          options[:items].<<(siblings) :
          options[:items][-1] = siblings      # Includes current line
      else
        return "| Menu begging for: #{beg}\n| (it returned #{txt.inspect})"
      end

      Expander.expand options
    end



    # Jump up to $... or =... line, and insert text
    def self.collapse_items_up_to_dollar txt, options={}
      Tree.to_parent

      line = Line.value
      while(line =~ /^\s/ && line !~ /^ *\$/ && line !~ /^ *=/) do
        Tree.to_parent
        line = Line.value
      end

      Tree.collapse :no_plus=>1
      Line.sub! /( *).*/, "\\1#{txt}"
      nil
    end

    # Called by .launch to do appriate thing if output starts with
    # =replace/, =open file/, =flash/, <!..., <<<..., or something else that
    # instructs the editor to take an action.
    def self.process_output_bullets txt, options={}, insert_options={}   # Based on bullets or =replace

      # Todo > delete if not used any more?

      # <$$ foo, so exit and run the command (if in shell console)...

      if txt =~ /\A<\$\$ /
        command = txt.sub /\A<\$\$ /, ''
        return View.<<("Don't know how to handle $$... when not run from shell.") if Environment.gui_emacs
        DiffLog.quit_and_run command
        return true
      end

      # <$ foo, so move output up to $... line and replaces it...
      # This might not be used any more > after making $... handle themselves

      if txt.strip =~ /\A<\$ (.+)\z/
        txt = $1

        self.collapse_items_up_to_dollar txt
        Launcher.launch
        return true
      end

      # <@ foo/, so replace item with output...

      if txt.strip =~ /\A<@ (.+)\z/
        # Not sure what this does > behaves as though it doesn't have a bullet?
        txt = $1
        indent = Line[/^ +=?/]   # Put '=' back on if it was there
        Line.sub! /.*/, "#{indent}#{txt}"
        return true
      end

      # <<< foo/, so replace item with output and launch...

      if txt.strip =~ /\A(<<+) (.+)\z/

        angles, txt = $1.length, $2

        # Extra angle brackets, so jump up and kill for each one...

        # (angles-3).times do
        (angles-2).times do
          break if Line.indent.length == 0
          Tree.to_parent
        end
        Tree.collapse if angles > 2

        indent = Line[/^ +=? ?/]   # Put '=' back on if it was there
        Line.sub! /.*/, "#{indent}#{txt}"

        Launcher.launch

        return true
      end

      # If padding at beginning, and one of these, do .unindent

      # <: and <+ replace siblings or line...

      if txt =~ /\A<([+:])\n/
        arg = $1
        to = arg == "+" ? "=replace/line/" : "=replace/siblings/"
        txt.sub! /../, to
      end

      # =replace/...

      if txt =~ /\A= ?replace\/(.*)/
        arg = $1

        # Remove the 1st line, and unindent the rest
        txt.sub! /\A=.+\n/, ''
        txt.gsub! /^  /, ''
        if arg == "neighbors/"
          raise "pass =replace/, instead of =replace/neighbors/"
        end

        expand_when_done = nil

        if arg == ""
          bounds = Tree.sibling_bounds
        elsif arg =~ /^siblings\/(.*)\/?/   # =replace/siblings/, or =replace/siblings/2/, or =replace/siblings/expand/
          arg = $1 == "" ? nil : $1
          if arg
            number, expand_when_done = arg.split "/"
            (number.to_i - 1).times{ Tree.to_parent }
            # Why would you launch when there's a number > Doesn't it just go up that many ancestors?
            # insert_options[:launch] = 1
          end
          bounds = Tree.sibling_bounds :cross_blank_lines=>1 #, :children=>1
        elsif arg == "quoted/"
          raise "implement quoted/!"
        elsif arg == "line/"
          bounds = Line.left, nil, nil, Line.right+1   # Just replace line
        else   # Just =replace/ ?
          raise "not implemented"
        end

        old_txt = View.txt bounds[0], bounds[3]
        indent = old_txt[/^ */]
        View.delete bounds[0], bounds[3]
        txt = "#{txt}".gsub(/^/, indent)

        Tree.output_and_search txt, insert_options.merge(:not_under=>1)

        return true
      end

      if txt =~ /\A=open file\/(.*)/

        filename = $1

        # Best place for special handling?

        self.open_file filename, options   #> |

        # Check preferences to see which editor to open it with...

        return true

      end

      # <* or =flash/, so flash message...

      if txt =~ /\A=(flash)\/(.*)/ || txt =~ /\A<(\*) ?(.*)/
        kind, txt = $1, $2
        txt = "- #{txt}" if kind == "!" && txt =~ /^[^-]/   # Add bullet if <* foo
        if txt.blank?
          Effects.glow :times=>1
        else
          View.flash txt
        end
        return true
      end

      # <? or =prompt/...

      if txt =~ /\A=prompt\/(.*)/ || txt =~ /\A<\? (.*)/
        View.prompt $1
        return true
      end

      nil
    end

    def self.specal_handling_for_file_extension filename

      extension = File.extname filename

      if [".jpg", ".jpeg", ".gif", ".png"].member?(extension)

        # Open with osx preview
        # Todo > find equivalent for linux distros?
        Shell.command %`qlmanage -p \"#{filename}\"`
        return true   # Tell caller to do nothing because we handled it
      end

      false   # Not a special file extension, so tell caller to handle it
    end


    def self.delete_option_item_siblings path=nil #, line=nil

      # Make options appearing and being deleted be part of the same undo chunk
      #   so one undo won't just show the options.
      View.remove_last_undo_boundary

      line = Line.value
      path ||= Tree.path rescue nil

      # Do nothing unless unless a parent line is ~...

      path = Path.split(path[-1]) rescue []

      index = path.index{|o| o =~ /\A[~*^] / && o !~ /\n/}

      return if ! index   # Do nothing if no ~... in path

      # Jump up to line that has ~...
      ((path.length - 1) - index).times { Tree.to_parent }

      bounds = Tree.sibling_bounds(:cross_blank_lines=>1, :must_match=>"\\^ |\\* ")

      at_left_margin = Line =~ /^[~*^]/

      # Also delete any non-tilde lines above, if not on blank line
      if ! at_left_margin
        # Maybe Find Better Way than calling .sibling_bounds twice
        bounds_of_all = Tree.sibling_bounds(:cross_blank_lines=>1)
        bounds[0] = bounds_of_all[0]
      end


      # Delete ~... siblings

      # raise "look for problem"


      txt = View.delete bounds[0], bounds[3]


      # At left margin

      if at_left_margin

        # At left margin, so delete linebreak after (if still blank)
        cursor = View.cursor
        if View.txt(cursor, cursor+1) == "\n"
          View.delete(cursor, cursor+1)
        end

      else
        # Not at left margin, so move up to item option was nested under
        Line.previous
      end

      if @@added_option_item_padding_above
        @@added_option_item_padding_above = nil
        $el.backward_delete_char(1) if View.txt(cursor-2, cursor) == "\n\n"
      end

      # Compare to see if it's back to unsaved state, and set unsaved if not
      if View.file   # Only do if > file exists

        diff = DiffLog.save_diffs(:just_return=>1) if View.file   # Only do if > file exists

        # If diff shows no changes, mark as unsaved!
        if ! diff || diff.count("\n") <= 2
          $el.set_buffer_modified_p nil
        end

      end

      txt

    end


    @@added_option_item_padding_above = nil
    def self.added_option_item_padding_above= val
      @@added_option_item_padding_above = val
    end


    def self.open_file txt, options={}

      # if |..., pull off line and go to it
      quote = Path.extract_quote(txt)

      if line_number = Path.extract_line_number(txt)
        line_number = line_number.to_i

        # Before we open, calculate difference between cursor's line and :... line number
        if char = Line[/^ +\|([+-])/, 1]
          siblings = Tree.siblings :before=>1
          line_number += siblings.select{|o| o =~ /^\[|:]#{Regexp.escape char}/}.length
        end
      end

      # Special handling for certain extensions like images > if no line number or quote
      return "" if self.specal_handling_for_file_extension txt

      open_options = options[:prefix] == 0 ? {:same_view=>1} : {}   # 0+ means use same view

      # Open with xsh, or editor specified in conf...

      conf_kind = options[:grab] ? "grab" : "expand"

      View.open txt, open_options

      # Check file prefs > if default editor set

      if line_number
        View.line = line_number
      elsif quote
        View.to_snippet quote.sub(/./, '')   # 1st char of quote is a redundant space or + or something else   #> ||
      end

    end

    # Editor-only handling for <<, <=, and <@ bullets.
    # For when launching <<... etc. lines, not for when output includes it.
    #
    # Also when more <'s, like <<< and <<= etc.
    def self.process_target_bullets_after line, txt, insert_options # , path, options

      arrow_bullet = line[/^ +(<[+:])/, 1]

      return nil if ! arrow_bullet   # Not handled

      key = arrow_bullet.sub /<+/, '<'

      indent = Line.indent line

      if key == "<+"
        # <+ foo, so delete current line before inserting...
        Line.delete
      else
        bounds = Tree.sibling_bounds(:cross_blank_lines=>1)
        # <: foo, so delete siblings before inserting...
        View.delete bounds[0], bounds[3]
      end

      txt.gsub! /^/, indent
      insert_options[:not_under] = 1

    end


    # How does this compare to > .process_output_bullets?
    #   - should they be merged?
    def self.process_target_bullets_before line

      arrow_bullet = line[/^ +(<[<=]+)/, 1]

      return nil if ! arrow_bullet   # Not handled

      key = arrow_bullet.sub /<+/, '<'


      case key
      when "<"
        # << foo, so replace parent before expanding...
        Command.launch_after_collapse
      when /<=/
        # <= foo, so replace all parents up to "=" before expanding...
        Command.launch_after_collapse_root
      end
      true   # We handled it
    end

    def self.insert_menu

      # Text selected, so nest selected text under the command...

      if View.selection?

        # Get bookmark from user

        command = Keys.input "Command to nest this text under: "

        left, right = View.range
        txt = View.delete left, right

        View.<< "#{command}/\n#{Tree.pipe txt, :indent=>"  "}", :dont_move=>1

        return
      end

      line = Line.value
      indent = Line.indent line
      blank = Line.blank?

      prefix = Keys.prefix

      if prefix == :u   # Indent 1 level less
        indent.sub! "  ", ""
      end


      # /foo/
      #   @bar/
      if prefix == 2
        Line << "#{Keys.bookmark_as_path}"
        Line << "\n  @"
        menu = Keys.input :timed=>1
        menu = "" if menu == " "
        Line << "#{menu}"
        Launcher.launch_or_collapse
        return
      end

      # /foo/
      #   @
      if prefix == 8
        Line << "#{Keys.bookmark_as_path}"
        Line << "\n  @"
        Launcher.launch_or_collapse
        return
      end

      # If line not blank, usually indent after

      Line.<<("\n#{indent}  = ") if ! blank

      # If at end of line, and line not blank, go to next line

      # Todo: if dash+, do auto-complete even if exact match - how to implement?

      prompt = "Start typing a command ('a' for all): "
      prompt.sub! ")", ", space for suggestions)" if ! blank
      input = Keys.input(:timed=>true, :prompt=>prompt)

      # If space, they want to do just raw "@", which will suggest something based on parent
      input = "" if input == " "

      View << input#+"/"

      Launcher.launch
    end


    def self.open_topic options={}
      insert = options[:insert]
      bm = options[:bm]

      if ! insert
        View.to_buffer View.unique_name("untitled.xiki")
        Notes.mode
        View >> "\n\n\n"
      end

      if ! bm
        View.flash(options[:as_command] ? "- Type a command quickly!" : "- Type a topic or command quickly!", :dont_nest=>1)
        bm = Keys.timed_insert :prompt=>"", :delay=>0.40
      end

      # Insert command from bookmark

      file = Bookmarks["%#{bm}"]

      if ! Notes.in_home_xiki_dir?(file)
        file = Notes.expand_link_file(file)
      end
      return if ! file

      topic = File.basename(file, ".*")
      topic.gsub! "_", " "
      Line.sub! /.*/, topic
      Launcher.launch

    end

    def self.open_prompt

      prompt = Keys.prefix_u ? "% " : "$ "

      View.to_buffer View.unique_name("untitled.xiki")
      Notes.mode

      View << "#{prompt}"
      View >> "\n\n\n"

    end

    # Mapped to jump+command.
    def self.open_nested_command

      dir = Keys.bookmark_as_path :prompt=>"Bookmark to run command under: "

      View.to_buffer View.unique_name("untitled.xiki")
      Notes.mode

      View << "#{dir}\n  = "
      View >> "\n\n\n"

      ControlLock.disable
    end

    def self.options_key
      Keys.remember_key_for_repeat ["task"]
      self.launch :task=>[]
    end

    def self.tasks_menu_on_bookmark options={}

      file = Keys.bookmark_as_path options.merge(:include_file=>1)

      return View.message("This view doesn't have a file.") if ! file

      # If not a dir, indent file under
      if file !~ /\/$/
        file.sub! /.+\//, "\\0\n  - "   # Indent before file
      end

      View.new_file
      View << file

      self.launch :task=>[]

    end

    def self.double_click

      # Line is heading, so show outline
      if Line =~ /^> /
        return FileTree.to_outline :not_topic_outline=>1
      end

      self.launch_or_collapse
    end

    def self.right_click options={}

      path = Tree.path

      expand_options = {:task=>[]}
      expand_options[:mouse] = 1 # unless options[:no_mouse]

      txt = Xiki.expand path, expand_options

      txt.unindent!
      txt_with_stars = txt.dup
      txt.gsub! /~ /, ""

      return if ! result

      self.launch :path_append=>"* #{result}"

    end

    # Jumps up to ^n and runs first "- )" line.
    def self.do_task options={}
      if Keys.prefix_u :clear=>1
        Code.load_this_file
      end

      # Update all Ol's
      Launcher.do_last_launch :nth=>1   #> |||||||||
      OlHelper.highlight_executed_lines
    end

    # Grabs path at cursor, and opens it in a new view.
    def self.expand_in_new_view

      path = Tree.path[-1]
      Launcher.open path

    end

  end

end

def require_menu file, options={}
  if ! options[:ok_if_not_found]
    raise "File Not Found" if !File.exists?(file)
  end

  stem = File.basename(file)[/\A(.*)\.[^.]+\z/, 1]

  # As .menu...

  if file =~ /\.menu$/ || options[:force_as] == "menu"
    Xiki::Launcher.add stem, :menu_file=>1 do |path|
      next Xiki::View.flash("- Xiki couldn't find: #{file}", :times=>5) if ! File.exists?(file)
      Xiki::Tree.children File.read(file), Xiki::Tree.rootless(path)
    end
    return
  end

  # As class, so require and add launcher...

  result = :not_found
  begin
    result = Xiki::Command.load_if_changed file
  rescue LoadError => e
    gem_name = Xiki::Requirer.extract_gem_from_exception e.to_s
    Xiki::Requirer.show "The file #{file} wants to use the '#{gem_name}' gem.\n% gem install #{gem_name}\n\n"
  rescue Exception=>e
    txt = Xiki::CodeTree.draw_exception e
    Xiki::Requirer.show "The file #{file} had this exception:\n#{txt}\n\n"
  end

  return if result == :not_found

  Xiki::Launcher.add stem if ! Xiki::Launcher.menus[1][stem]
end
