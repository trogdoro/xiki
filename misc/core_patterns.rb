# Defines default patterns

load "#{Xiki.dir}roots/mysql/mysql_index.rb"

module Xiki
  # Menu::Mysql.def_patterns

  # $..., %... or &... shell commands...

  Xiki.def(/\A([$%&])( |$| ?\/).*\z/) do |path, options|

    ancestors, dir = options[:ancestors], options[:dir]

    if dir =~ /^\/\w+@/
      next Remote.expand dir, options.merge(:command=>path)
    end

    # "* task" ancestor above this prompt, so collapse up parent command and replace it...

    if ancestors && (last = Path.split ancestors[-1]) && (last.find{|o| o =~ /^\* /})

      # This block is for when you have something like:
      # $ foo
      #   * recent/
      #     $ foo bar

      # Grab this line, then collapse up to before the tasks
      command = Line.without_label

      Tree.to_parent
      Tree.collapse

      # Delete up until and including ~... level
      Launcher.delete_option_item_siblings

      line = Line.without_label

      # Line is $..., so replace it
      if line =~ /^\$( | ?$)/
        Line.sub! /( *).*/, "\\1#{command}"
      else
        # Line not $..., so insert $... underneath
        Line.sub! /( *).*/, "\\0\n\\1  #{command}"
        Line.next
      end

      next Launcher.launch

    end


    # Normal parent root, so put command under it...

    prompt = path[/^[$%&]/]

    command = path[/^..(.+)/, 1]

    if command
      command.sub! /^\//, ''
      command = Path.split command
      options[:command] = command.shift

      options[:args] = command if command.any?
    end

    # '% foo' with no children, so Change to '$ foo' And relaunch
    if prompt == "%" && options[:args] == nil
      Line.sub! "%", "$"
      next Launcher.launch
    end

    Shell.shell_command_per_prompt prompt, options   #> ||||||||||||||||||||
  end

  # http://foo.com...

  Xiki.def(%r"\A(https?://[^/]|file://)") do |path, options|
    # "url/| Quote", so pull off last item as text to post

    list = Path.split path
    if list[-1] =~ /\n/
      post_txt = list.pop
      path = Path.join list
    end

    task = options[:task]

    if task == []
      next "
        * browser
        * source
        * no useragent
        * pretty json
        * text from page
        "
    end

    Launcher.append_log path

    # source/ item on separate line, so insert the source...

    prefix = Keys.prefix
    Keys.clear_prefix

    url = path[/(http|file).?:\/\/.+/]


    if task == ["text from page"]
      txt = Shell.command "lynx --dump \"#{url}\""
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')
      next txt.snippet
    end


    # Make this look for "* source/" in path?
    if post_txt || prefix == "all" || task == ["source"] || task == ["no useragent"] || task == ["pretty json"]

      options = {}
      if ! task || task != ["no useragent"]
        options[:headers] = {"User-Agent"=>"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.71 Safari/537.36"}
      end

      if post_txt
        options[:body] = post_txt
        txt = RestTree.post url, options
      else
        txt = RestTree.get url, options
      end

      if task == ["pretty json"]
        txt = JSON[txt]
        txt = JSON.pretty_generate txt
      end

      txt = Tree.quote(txt) if txt =~ /\A<\w/
      next Tree.quote txt
    end

    url.gsub! '%', '%25'
    url.gsub! '"', '%22'

    Browser.url url, :os_open=>1

    options[:halt] = 1   # Don't do any other expanders

    "<*"
  end

  # Special launching for "ol" view > output log...

  Xiki.def(//, :pre=>1, :target_view=>"ol") do |path, options|
    # bug > this doesn't get control when > root is '/foo' > passes control to FileTree
    options[:halt] = 1
    OlHelper.launch
    Effects.blink :what=>:line
    nil
  end

  # Comments in .rb files (often Foo.bar)
  Xiki.def(//, :pre=>1, :target_extension=>".rb") do |path, options|

    line = Line.value
    indent_orig = Line.indent line

    next unless line =~ /^ *#/

    options[:halt] = 1

    # Eval comment inline
    line.sub! /^ *# */, ''

    txt = Xiki[line, options.select{|key, value| [:ctrlx, :task].include?(key)}]   #> |||||||||||||

    next nil if txt.blank?

    txt.gsub!(/^/, "#{indent_orig}#   ")
    txt.sub!(/\n+\Z/, "")

    Line.<< "\n#{txt}", :dont_move=>1

    nil
  end

  # !... lines (at left margin)

  Xiki.def(/\A!( |$|\/)/) do |path, options|

    path_split = Path.split(path)

    # No children, so just use siblings
    if path_split.length == 1
      code = Tree.siblings.select{|o| o =~ /^! /}.join("\n")
    else
      # Jump to !... ancestor
      cursor = View.cursor
      while Line =~ /^ *[^ !\n]/
        Tree.to_parent
      end

      raise "Not under !... line" if Line !~ /^ *!/

      code = Tree.siblings.grep(/^!/).join("\n")

      View.cursor = cursor
    end

    code.gsub! /^! ?/, ''

    if options[:client] =~ /^editor/
      # Calculate where we are in relation to the parent, for stack trace
      line_number = View.line
      lines_above = Tree.siblings(:before=>1).select{|o| o =~ /^! /}
      line_number -= lines_above.length
    else
      line_number = 0
    end

    returned = Code.eval code, View.file, line_number, {:pretty_exception=>1, :simple=>1}, {:args=>path_split[1..-1]}

  end

  # a b c, so treat as music notes...

  Xiki.def(/\A[a-z][ #]([a-z ][ #])+[a-z ]#?\z/i) do |path, options|

    options[:halt] = 1

    Kernel.require "#{Xiki.dir}roots/piano"

    Piano.song path

    ""
  end

  # Various lines that mean run as ruby
  # p ...
  # puts ...
  # etc.
  Xiki.def(/\A(p|pp|print|puts) /) do |line|
    # Don't quote temporarily, for presentation
    CodeTree.run line#, :quote=>1
    nil
  end

  Xiki.def(/^ap /) do |line|
    returned, txt, exception = Code.eval line, :pretty_exception=>1

    txt.sub!(/\A{\n/, '')
    txt.sub!(/\n}\n\z/, '')
    Tree.pipe txt
  end

  Xiki.def(/^print\(/) do |line|
    Javascript.launch
  end

  Xiki.def(/^pg /) do |line|
    line.sub! /^pg /, ''
    CodeTree.run "puts JSON.pretty_generate(#{line})"
  end

  # GET http://foo.com
  #  and
  # GET /regex/ http://foo.com
  Xiki.def(/^(- )?GET /) do |path, options|
    url = path.sub(/^GET /, '')
    regex = url.slice! %r"^/.+?/ "

    url = "http://#{url}" unless url =~ %r"http://"
    url.gsub! ' ', '+'

    html = RestTree.request "GET", url

    if regex
      regex = regex[%r"/(.*)/ $", 1]   # "
      html = html.split("\n").grep(/#{regex}/i).join("\n")
    end

    html = Tree.quote(html) if html !~ /^[>|] / && html !~ /\A.+\/$/
    html
  end

  Xiki.def(%r"^source://") do |path, options|
    path.sub! /^source/, 'http'
    path.gsub! ' ', '+'
    Tree.quote `curl -A "Mozilla/5.0" #{path}`
  end

  # xiki://... url...

  Xiki.def(%r"^xiki://") do |path, options|

    path = URI.encode(path.gsub(" ", '-'))   #> "http://xiki.loc/nova/> Bang/Here are some elements;lfor you.;l;l;l"


    RestTree.xiki_url path, options
  end

  # http//foo.com, so show result...

  # (only one slash means show result)
  # Xiki.def(%r"^http:/.+") do |url, options|
  Xiki.def(%r"\Ahttp:(/|///)[^/]") do |url, options|
    url.sub! %r"/+", "//"
    url.gsub! ' ', '+'

    html = RestTree.request "GET", url

    html = Tree.quote(html) if html !~ /^[>|] / && html !~ /\A.+\/$/
    html
  end

  # # comment...

  Xiki.def(%r"\A# (\w+)") do |path, options|

    split = Path.split(path)

    X"%xiki/###{path}/", :client=>"editor"

  end

  # #css_id...

  Xiki.def(%r"\A#($|\w+)") do |path, options|
    next Xiki["ids/"].gsub(/^\+/, "<<") if path == "#"
    Xiki["ids/#{path}", options]
  end

  # Foo.bar, so invoke method on class...
  Xiki.def(%r'\AX[" ]|\A[A-Z][:A-Za-z]+\.[a-z]') do |path, options|

    # Eval comment inline
    path.sub! /^ *# */, ''
    txt, out, exception = Code.eval path, View.file, Line.number, :pretty_exception=>1
    txt = CodeTree.returned_to_s txt

    next if ! txt && out == "" && ! exception # Do nothing if no return value or output

    txt ||= out
    txt = exception ? exception.strip : txt.to_s
    txt
  end

  # Foo. ...
  Xiki.def(%r"\A[A-Z][A-Za-z:]+\.(\/|\z)") do |path, options|

    path = path.split '/'

    path[0].sub!(/\.$/, '')

    txt = Meths.list *path
    txt = txt.map{|o| "- #{o}\n"}.join('') if txt.is_a?(Array)
    txt
  end

  # Matches lines from el4r log like:
  # 23:25:55:095  Error:
  #   from /Users/steve/.rvm/gems/ruby-1.9.3-p327@xiki/gems/trogdoro-el4r-1.0.7/bin/el4r-instance:847:in `instance_eval'

  Xiki.def(%r"^\d\d:\d\d:\d\d:\d\d\d  .+/from (/.+):(\d+):") do |path, options|
    match = options[:expanders][0][:match]
    file, line = match[1..2]

    View.open file
    View.line = line
    nil
  end

  # foo+bar ? ...

  Xiki.def(/^[a-z]+\+[a-z.+]*.?(\/|$)/) do |path, options|
    Keys.expand_plus_syntax Path.split(path), options
  end

  # foo.com > urls...

  Xiki.def(/\A[a-z][a-z0-9.-]+\.(org|com|edu|net|co|cc|in|de|loc|us)(:\d+)?(\/|\z)/) do |path, options|


    split = Path.split path

    options.delete :no_slash
    url = "xiki://#{path}"
    if items = options[:items]
      url = FileTree.add_slash_maybe url
      url += items.join("/")
    end

    Xiki.expand url#, :post=>post
  end

  # localhost...

  Xiki.def(/\Alocalhost(:\d+)?(\/|\z)/) do |path|
    Xiki.expand "xiki://#{path}"
  end

  # foo@bar.com remote path...

  Xiki.def(/\A\/\w+@.+\z/) do |path, options|
    Remote.expand path, options
  end


  Xiki.def(/\A\+\w[\w ]+(\z|\/)/) do |path, options|
    Command.expand_define path, options
  end


  # :search...

  Xiki.def(/\A\:[a-z][\w -]*(\/|\z)/i) do |path, options|
    path = path.sub(/\A:/, '')

    # Add args to the path

    path = Path.split(path)
    path += options[:items] if options[:items]

    Xiki["search", path, options]
  end



  # .xiki...

  # Xiki.def(/\A\:([\w -]+)/) do |path, options|
  Xiki.def(/\A_([\w -]+)/) do |path, options|

    # New > call .drill directly > works
    items = Path.split path

    # name = items.slice!(0).sub(/^:/, '')
    name = items.slice!(0).sub(/^_/, '')
    name.gsub! " ", "_"

    path = File.expand_path("~/xiki/#{name}.xiki")

    # Use .link if not found and it exists
    if ! File.exists?(path) && File.exists?(found = path.sub(/\.xiki$/, ".link"))
      path = found
    end

    txt = Notes.drill path, *items, options

  end


  # @ (by itself)...

  Xiki.def(/\A@\z/) do |path, options|
    "
    | Use only first names?
    | - how to integrate with =contacts?
    |   - Make it look in it optionally?
    |   - use it only?
    |     - and have full names be in parens?
    |       =contacts/
    |         > Steve Jones (steve)
    |         > Ed Smith
    "

    # /name/, so make parent be @...

    Xiki["mail"].gsub(/^- /, "<< @").gsub(/^=.+\n/, "")

  end


  # NNN-NNN-NNNN, so treat as phone number...

  Xiki.def(/\A\d{3}-\d{3}-\d{4}/) do |path, options|

    Launcher.append_log path

    Xiki.expand "twilio/#{path}", options.select{|k, v| [:task].include?(k) }
  end


  # :@foo, so remove colon...

  Xiki.def(/\A:@\w.*/) do |path, options|
    Line.sub! /^:/, ''
    Launcher.launch
  end


  # @foo, so treat xikihub user...

  Xiki.def(/\A@\w.*/) do |path, options|

    options[:halt] = 1

    XikihubClient.user path, options

  end



  # email@address.com Message...

  Xiki.def(%r"^[a-z][a-z.]+@[a-z][a-z.]+[a-z] ") do |path, options|

    email, subject = path.split(" ", 2)
    section = Notes.current_section_object.text

    # New session, so move to top of 'misc' topic first

    XikihubClient.message email, subject, section, options

    options[:halt] = 1
    "<* - Message sent"
  end


  # > foo $bar...

  Xiki.def(/^> .+ \$\w+:?$/) do |path|
    path = path[/\$\w+/]
    View.open path
  end

  # # alert(... > run in the browser...

  # 7af8ec8 > git commit hashes...

  Xiki.def(/\A[0-9a-f]{7}(\/|\z)/) do |path|
    Xiki.expand "%xiki/=git/log/#{path}"
  end
  Xiki.def(/\A[0-9a-f]{40}(\/|\z)/) do |path|
    Xiki.expand "%xiki/=git/log/#{path}"
  end

  # ... : ..., so maybe memorize table...

  Xiki.def(/\A[^\n; ]+ : [^\n; ]+\z/) do |path, options|

    # Do nothing if already handled by a different pattern
    next if options[:expanders].length > 1

    # right-clicked root, so show "memorize" option...

    next "* memorize\n* save to Memorize.com" if options[:task] == []
    Kernel.require "#{Xiki.dir}roots/memorize"
    next Memorize.tasks_memorize if options[:task] == ["memorize"]
    next Memorize.tasks_save_to_memorize if options[:task] == ["save to Memorize.com"]

    nil
  end

  # :-... or :+..., so save as before and after...

  Xiki.def(%r"^:[?+-]") do |path, options|

    txt = Tree.siblings :quotes=>":", :string=>1
    before = txt.scan(/^[?-].+/).join("\n").gsub(/^./, '')
    after = txt.scan(/^\+.+/).join("\n").gsub(/^./, '')

    task = options[:task]

    # ~, so return options...

    next "* file search\n* broad search\n* filename search" if task == []
    if task && task[0] =~ /search/

      # Pull search string string from current line

      txt = Line.value.sub(/^:[+-?]/, '')

      # Use red as search, or green if no red

      file = Keys.bookmark_as_path :prompt=>"Enter a bookmark to search in: "
      file = Files.tilde_for_home file
      View.to_after_bar

      # ~ flexible search, so turn before into a regex
      filter = task == ["broad search"] || task == ["filename search"] ?
        TextUtil.regex_case(txt) :
        Regexp.quote(txt)

      if task == ["filename search"]
        FileTree.grep_with_hashes file, filter, '**'   # Open buffer and search
        next ""
      end

      FileTree.grep_with_hashes file, filter

      next ""
    end


    # :+foo, so just save...

    Clipboard.register("1", before)
    Clipboard.register("2", after)

    "<* Saved these as 'before' and 'after'"
  end

  # ^foo, so must not have been handled as a file, so say bookmark doesn't exist...

  Xiki.def(%r"^%[a-z]+(/|$)") do |path, options|
    # Note that this only runs if FileHandler doesn't handle it already
    "<* Bookmark doesn't exist: #{path}"
  end

  # .. or ..., etc, so replace with absolute dir, n-1 higher...

  Xiki.def(/^\.\.+$/) do |path, options|
    dir = View.dir

    # Chop off one dir for each dot (minus 1)
    (path.length-1).times do
      dir.sub! /\/[^\/]+$/, ''
    end

    "<< #{dir}"
  end


  # Todo > Probably bring this back
  #
  # # %foo/, so filter output for pattern...
  #
  # Xiki.def(/^%[^ \n]/) do |path, options|
  #   path = path[/\A%(.+)\/?/, 1]
  #   txt = Xiki.expand "filter/#{path}", options.select{|k, v| [:task, :ancestors].include?(k) }
  #   txt
  # end


  # ##foo/, so prompt for bookmark and do search inline...

  Xiki.def(/\A(##|\*\*).+\z/) do |path, options|

    # Idea > if ends in "/", look in filenames instead of contents

    # ~, so show task to do search in inline...

    next "* inline" if options[:task] == []

    symbols = path[/##|\*\*/]

    # path.sub! /^##/, ""
    path.sub! symbols, ""
    path.sub! /\/$/, ""

    # Prompt for bookmark...

    file = Keys.bookmark_as_path :prompt=>"Enter a bookmark to search in: ", :include_file=>1
    file = Files.tilde_for_home file

    # "* inline" task, so open in inline...

    if options[:task] == ["inline"]
      Line.sub! /.+/, "#{file}\n  + #{symbols}#{path}"
      Move.next
      Launcher.launch
      next
    end

    # No task, so just do in new view...

    View.to_after_bar
    FileTree.grep_with_hashes(file, path, symbols)

    ""
  end

  # > Heading, so render dropdown for moving and deleting section...

  Xiki.def(/\A>( |\z)/i) do |path, options|

    args, task = options[:args], options[:task]

    # No items /*, so show root tasks...

    if ! args && task # || task == []

      task[0] = "* #{task[0]}" if task[0]

      result = Xik.new("
        * delete
          ! Notes.cut_block :no_clipboard
        * cut
          ! Notes.cut_block
        * copy
          ! Notes.copy_block

        * to top
          ! Notes.move_block_to_top
        * previous
          ! Notes.move_block :up=>1
        * next
          ! # failed > # View.remove_last_undo_boundary
          ! Notes.move_block
      ").expand(task, :eval=>1)

    end

  end

  # "topic >> Heading", so navigate to it...

  Xiki.def(/\A[a-z][a-z0-9 ]+ ?>>/i) do |path, options|
    path = Path.split path

    topic = path[0][/(.+?) ?>> ?/, 1]

    path[0].sub!(/^.+? ?>> ?/, "\\1> ")

    options[:task] = ["navigate"]

    options.delete :expanders
    options.delete :path

    Xiki["#{topic}/", path, options]

    options[:halt] = 1   # Don't let "foo > bar" pattern go next

    ""
  end


  # ":search > Heading", so delegate to notes/ command...

  Xiki.def(/\A:[a-z][a-z0-9 ]+ ?>/i) do |path, options|

    path = Path.split path

    # Don't do > because it won't escape crazy chars in heading

    topic = path[0][/:(.+?) ?> ?/, 1]
    path[0].sub!(/^.+? ?> ?/, "\\1> ")

    # Delegate to topic
    # Delegate to path
    result = Xiki[":#{topic}/", path, options]

    result || "<* - not found, ^O to create"
  end

  # "topic > Heading", so delegate to notes/ command...

  Xiki.def(/\A[a-z][a-z0-9 ]+ ?>/i) do |path, options|
    path = Path.split path

    topic = path[0][/(.+?) ?> ?/, 1]
    path[0].sub!(/^.+? ?> ?/, "\\1> ")

    # Delegate to topic
    result = Xiki["#{topic}/", path, options]
    result || "<* - not found, ^O to create"
  end


  Xiki.def(/\A[.~]\z/i) do |path, options|
    Line.add_slash
    Launcher.launch
  end



  # "1. Foo", treat as "-> Foo:", plus "* update numbers"...
  Xiki.def(/\A\d+\.(\z| .*\z)/) do |path, options|

    task = options[:task]

    words = path[/. (.+)/, 1]   #> !!!

    # Ctrl+T, so get tasks from "->" and add on
    if task
      # Use tasks from "-> foo", adding our own
      if task == []
        next Xiki["-> #{words}", options] + "\n* update numbers"
        next "* update numbers"
      end
      # Handle if > * update numbers
      if task == ["update numbers"]
        Notes.fix_numbers
        next ""
      end
    end

    # Delegate to > "-> Foo:"
    Xiki["-> #{words}:", options]

  end

  # "#+", show all #+... comments in > xiki...
  Xiki.def(/\A#\+\z/i) do |path, options|
    Launcher.open("#{Bookmarks["%s"]}\n  - ##^ *#\\+/")
  end

  # "#++", show all #+... comments in > xiki hub...
  Xiki.def(/\A#\+\+\z/i) do |path, options|
    Launcher.open("#{Bookmarks["%h"]}\n  - ##^ *#\\+/")
  end



  # "->", so list headings in same view...

  Xiki.def(/\A->(\/|$)/i) do |path, options|

    # -> only, so list the headings

    if path == "->"

      file = View.file
      return if ! file

      txt = File.read file
      txt.encode!('UTF-8', 'binary', :invalid=>:replace, :undef=>:replace, :replace=>'')
      headings = txt.scan(/^> [^:\n].*/)
      next headings.join("\n")
    end

    Tree.to_parent
    Tree.collapse

    path = Path.split path
    Line.sub! /.*/, "-#{path[1]}"

    ""

  end

  # "-> Heading", so jump to heading in same view...

  Xiki.def(/\A-> /i) do |path, options|
    path.sub! /^-> /, ''

    # Show and handle tasks
    task = options[:task]
    next "* create" if task == []
    if task == ["create"]

      View.to_top
      # Notes.to_block :prefix=>:u

      View << "> #{path}\n"
      View >> "\n\n\n\n"

      next ""
    end


    # Search below for this heading

    orig = View.cursor   #> nil
    result = Search.forward "^> \\(@[a-z]* \\)?#{Search.quote_elisp_regex path}$", :from_top=>1, :beginning=>1

    if ! result
      View.cursor = orig
      View.flash "- Heading not found in this view > Ctrl+T to create!"
      next
    end

    View.recenter_top

    Move.down

    nil
  end


  # "<- topic" or "<- topic > Heading", so jump to heading in same view...

  Xiki.def(/\A<- /i) do |path, options|

    # path = "<- protonight > Foo > Bar"
    # path = "<- protonight > Just the emails"
    # path = "<- protonight"
    path.sub! /^<- /, ''
    topic, heading = path.split(" > ", 2)

    # Jump to topic > if it exists
    Xiki[path, :task=>["view source"]]

  end

  # ".foo", so call the heading in this file...

  Xiki.def(/\A\.[a-z]/i) do |path, options|

    path = path.sub /^\./, ''   # Remove dot at beginning

    pathified = View.file.sub(/\.xiki$/, '//')   #> "/Users/craig/xiki/notes.xiki"

    options.keys.each do |key|
      options.delete key if ! [:items, :task, :path, :ancestors].member? key
    end

    # May cause problems > passing items on!
    Xiki["#{pathified}#{path}", options[:items], options]   #> |||||||||
  end

  # "foo.../", so treat as autocomplete

  Xiki.def(/\A[a-z][a-z ]*\.\.\.\//i) do |path, options|

    # Only replace subsequent '  foo' siblings, in case autocompleting under something else!!"   #> # Only replace subsequent '  foo' siblings, in case autocompleting under something else

    bounds = Tree.sibling_bounds(:must_match=>"[a-z]")
    View.delete bounds[0], bounds[-1]
    path = Path.split path

    Move.up
    Line.gsub! /.+/, path[-1]

    Launcher.launch

    nil
  end

  # Just "foo...", so do same as Tab on "foo"...

  Xiki.def(/\A[a-z][a-z ]*\.\.\.\z/i) do |path, options|
    # "todo > treat like tab"
    Topic.tab_key :flashing_ok=>1
    nil
  end


  # "#: foo", so search for "foo" below...

  Xiki.def(/\A#:(.+)/) do |path, options|
    match = options[:expanders][0][:match][1]

    Line.to_right
    Search.forward match
    # "todo > treat like tab"
    nil
  end

end
