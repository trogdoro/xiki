# Defines default patterns

load "#{Xiki.dir}commands/mysql/mysql_index.rb"

module Xiki
  Menu::Mysql.def_patterns

  # $$... shell commands (exit and run)...

  Xiki.def(/^\$\$ .+/) do |path, options|
    path.sub! /^\$\$ /, ''
    Xsh.exit_and_run path
    ""
  end

  # $... or %... shell commands (when not under dir)...

  Xiki.def(/^([$%&])( |$| ?\/)/) do |path, options|
    prompt = path[/^[$%&]/]
    command = path[/^..(.+)/, 1]
    if command
      command.sub! /^\//, ''
      command = Path.split command
      options[:command] = command.shift
      options[:args] = command if command.any?
    end

    Shell.shell_command_per_prompt prompt, options
  end

  # http://example.com...

  Xiki.def(%r"^(https?://[^/]|file://)") do |path, options|

    if options[:dropdown] == []
      next "
        ~ source/
        ~ browser/
        "
    end

    Launcher.append_log path

    # source/ item on separate line, so insert the source...

    prefix = Keys.prefix
    Keys.clear_prefix

    url = path[/(http|file).?:\/\/.+/]

    # Make this look for "~ source/" in path?
    if prefix == "all" || options[:dropdown] == ["source"]
      gem 'httparty'; Kernel.require 'httparty'
      txt = HTTParty.get(url).body

      txt = Tree.quote(txt) if txt =~ /\A<\w/
      Tree.under Tree.quote(txt), :no_slash=>1
      next
    end
    url.gsub! '%', '%25'
    url.gsub! '"', '%22'
    prefix == :u ? $el.browse_url(url) : Firefox.url(url)

    "<!"
  end

  # Special launching for "*ol" view
  Xiki.def(//, :pre=>1, :target_view=>"*ol") do |path, options|
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

    txt = Xiki[line]

    next nil if txt.blank?

    txt.gsub!(/^/, "#{indent_orig}#   ")
    txt.sub!(/\n+\Z/, "")

    Line.<< "\n#{txt}", :dont_move=>1

    nil
  end

  # !... lines (at left margin)
  Xiki.def(/^! /) do |path, options|

    code =
      if options[:client] !~ /^editor/   # If not in an editor, just use the path
        path
      elsif Line =~ /^ *@/   # @!..., so just use one line
        Line[/! .+/]
      else   # !..., so use multiple lines
        Tree.siblings.select{|o| o =~ /^! /}.join("\n")
      end

    code.gsub! /^@?! /, ''

    if options[:client] =~ /^editor/
      # Calculate where we are in relation to the parent, for stack trace
      line_number = View.line
      lines_above = Tree.siblings(:before=>1).select{|o| o =~ /^! /}
      line_number -= lines_above.length
    else
      line_number = 0
    end

    returned, out, exception = Code.eval code, View.file, line_number, :pretty_exception=>1
    next exception if exception
    returned ||= out   # Use output if nothing returned
    returned = returned.to_s if returned

    next if returned.blank?
    Tree.quote returned
  end

  # a b c, so treat as music notes...

  Xiki.def(/\A[a-z][ #]([a-z ][ #])+[a-z ]#?\z/i) do |path, options|

    options[:halt] = 1

    Kernel.require "#{Xiki.dir}commands/piano"

    Piano.song path

    ""
  end

  # Various lines that mean run as ruby
  # p ...
  # puts ...
  # etc.
  Xiki.def(/^(a |Ol[ \[])/) do |line|
    line.sub! /^a /, "Ol.a "
    CodeTree.run line, :quote=>1
    "=flash/"
  end

  Xiki.def(/^(p|pp|print|puts) /) do |line|
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

  Xiki.def(%r"^xiki://") do |path, options|
    RestTree.xiki_url path
  end

  # http:/foo.com (only one slash means show result)
  # Xiki.def(%r"^http:/.+") do |url, options|
  Xiki.def(%r"^http:(/|///)[^/]") do |url, options|
    url.sub! %r"/+", "//"
    url.gsub! ' ', '+'

    html = RestTree.request "GET", url

    html = Tree.quote(html) if html !~ /^[>|] / && html !~ /\A.+\/$/
    html
  end

  # # comment...

  Xiki.def(%r"\A# (\w+)") do |path, options|

    split = Path.split(path)

    X":xiki/###{path}/", :client=>"editor"

  end

  # #css_id...

  Xiki.def(%r"\A#($|\w+)") do |path, options|
    next Xiki["ids/"].gsub(/^\+/, "<<") if path == "#"
    Xiki["ids/#{path}"]
  end


  # # .css_class ...
  # Xiki.def(%r"^\.(\w+|)$") do |path, options|   # "
  #   next Xiki["css/list/"].gsub(/^\+/, "<<") if path == "."

  #   Xiki["css/list/#{path}"]
  # end

  # Foo.bar...
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

  Xiki.def(/^[a-z]+\+[a-z.+]*.?(\/|$)/) do |path, options|
    Keys.expand_plus_syntax Path.split(path), options
  end

  Xiki.def(/\A[.a-z]+\.(org|com|edu|net|co|cc|in|de|loc)(:\d+)?(\/|\z)/) do |path, options|
    options.delete :no_slash
    Xiki.expand "xiki://#{path}"
  end

  Xiki.def(/\Alocalhost(:\d+)?(\/|\z)/) do |path|
    Xiki.expand "xiki://#{path}"
  end

  Xiki.def(/^\/\w+@/) do |path, options|
    Xiki::Remote.expand path, options
  end

  # ^ (by itself), so list all notes...

  Xiki.def(/^\^$/) do |path, options|
    txt = Xiki.expand "notes"
    txt.gsub(/^\+ /, "<< ^")
  end

  Xiki.def(/\A\+\w[\w ]+(\z|\/)/) do |path, options|
    Menu.expand_define path, options
  end

  # ^notes...

  Xiki.def(/\A\^([\w -]+)/) do |path, options|
    name = options[:expanders].find{|o| o[:match]}[:match][1]
    options.delete :no_slash if options[:path] !~ /\//
    Xiki.expand options[:path].sub(/^\^/, "notes/"), options.select{|key, value| [:prefix, :dropdown].include?(key)}
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

    Xiki.expand "twilio/#{path}", options.select{|k, v| [:dropdown].include?(k) }
  end

  # @foo, so treat as tweet...

  Xiki.def(/\A@\w.*/) do |path, options|

    # @foo message, so send message as tweet...

    if path =~ / .+/
      path.gsub!(/([^a-z0-9_-])/i){ "\\#{$1}" }
      txt = "t update #{path}"
      result = Shell.sync txt
      next result if result =~ /^> error/

      next "<! Tweet sent!"
    end

    "
      - email/
        > Subject
        | Here's an email address
      - text message/
      - call/
      - tweets/
        - twitter/
          - tweet to/
          - dm/
          - profile/
            ! t whois keithtom
      | Misc notes can go here.
      | Expanding just saves them.
      | user@example.com
      | @twittername
      | 523-288-2013
    "
  end

  # email@address.com, so delegate to =mail...

  Xiki.def(/\A[a-z0-9._-]+@[a-z0-9.-]+\.[a-z]+(\/|$)/i) do |path, options|
    email = options[:expanders].find{|o| o[:match]}[:match][0]

    path = Path.split options[:path]   # => ["craig.muth@gmail.com", "The body of\nthe email.\n"]

    # Dropdown...

    if dropdown = options[:dropdown]

      # No items, so list templates...

      txt = File.read File.expand_path "~/xiki/commands/email_templates.notes"
      array = txt.scan(/^> (.+?)\n([^>]+)/m).flatten
      emails = Hash[*array]

      if dropdown == []
        # next txt.scan(/^> (.+)/).map{|o| "~ #{o[0]}/\n"}.join
        next emails.keys.map{|o| "~ #{o}/\n"}.join
      end

      txt = emails[dropdown[0]]
      txt.gsub! /^/, "| "
      txt.sub! /\| - subject: /, "> "
      next txt

    end

    Xiki["mail/#{email}", path[1..-1]]
  end

  # > foo $bar...

  Xiki.def(/^> .+ \$\w+:?$/) do |path|
    path = path[/\$\w+/]
    View.open path
  end

  # # alert(... > run in the browser...

  # 7af8ec8 > git commit hashes...

  Xiki.def(/\A[0-9a-f]{7}(\/|\z)/) do |path|
    Xiki.expand ":xiki/=git/log/#{path}"
  end
  Xiki.def(/\A[0-9a-f]{40}(\/|\z)/) do |path|
    Xiki.expand ":xiki/=git/log/#{path}"
  end

  # ... : ..., so maybe memorize table...

  Xiki.def(/\A[^\n; ]+ : [^\n; ]+\z/) do |path, options|

    # Do nothing if already handled by a different pattern
    next if options[:expanders].length > 1

    # right-clicked root, so show "memorize" option...

    next "~ memorize\n~ save to Memorize.com" if options[:dropdown] == []
    Kernel.require "#{Xiki.dir}commands/memorize"
    next Memorize.dropdown_memorize if options[:dropdown] == ["memorize"]
    next Memorize.dropdown_save_to_memorize if options[:dropdown] == ["save to Memorize.com"]

    nil
  end

  # .foo > call command declared in same file...

  # TODO > remove ".foo" convention, and look for local command if "foo" (make them have precedence over global commands)
  Xiki.def(%r"^\.(\w+)(\/|$)") do |path, options|   # "
    Launcher.launch_local path, options
  end

  # :-... or :+..., so save as before and after...

  Xiki.def(%r"^:[+-]") do |path, options|

    txt = Tree.siblings :quotes=>":", :string=>1
    before = txt.scan(/^-.+/).join("\n").gsub(/^-/, '')
    after = txt.scan(/^\+.+/).join("\n").gsub(/^\+/, '')
    Clipboard.register("1", before)
    Clipboard.register("2", after)

    "<! Saved these as 'before' and 'after'"
  end

  # :foo, so must not have been handled as a file, so say bookmark doesn't exist...

  Xiki.def(%r"^:[a-z]+(/|$)") do |path, options|
    # Note that this only runs if FileHandler doesn't handle it already
    "<! Bookmark doesn't exist"
  end

end
