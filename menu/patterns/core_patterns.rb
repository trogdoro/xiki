# Necessary since it's lazy loaded
# Probably pull common stuff out into pre-loaded class
  # Maybe into class in $x/menu/patterns?
load "#{Xiki.dir}menu/mysql/mysql_index.rb"

module Xiki
  Menu::Mysql.def_patterns

  # $... shell command lines when not under dir
  Xiki.def(/^([$%&]) (.+)/) do |path, options|
    match = options[:expanders].find{|o| o[:match]}[:match]
    prompt, command = match[1..2]
    options[:command] = command

    Console.shell_command_per_prompt prompt, options
  end

  # Url's
  Xiki.def(%r"^(https?://[^/]|file://)") do |path, options|

    # Todo: make this work when no editor

    Launcher.append_log path

    prefix = Keys.prefix
    Keys.clear_prefix

    url = path[/(http|file).?:\/\/.+/]
    if prefix == "all"
      txt = HTTParty.get(url).body

      txt = Tree.quote(txt) if txt =~ /\A<\w/
      Tree.under Tree.quote(txt), :no_slash=>1
      next
    end
    url.gsub! '%', '%25'
    url.gsub! '"', '%22'
    prefix == :u ? $el.browse_url(url) : Firefox.url(url)
    nil
  end

  # Special launching for "*ol" view
  Xiki.def(//, :pre=>1, :target_view=>"*ol") do |path, options|
    options[:halt] = 1
    OlHelper.launch
    Effects.blink :what=>:line
    nil
  end

  # Comments in .rb files
  Xiki.def(//, :pre=>1, :extension=>".rb") do |path, options|

    line = Line.value
    next unless line =~ /^ *#/

    options[:halt] = 1

    # Eval comment inline
    line.sub! /^ *# */, ''
    txt, out, exception = Code.eval line, View.file, Line.number, :pretty_exception=>1

    next if ! txt && out == "" && ! exception # Do nothing if no return value or output

    txt ||= out
    txt = exception ? exception.strip : txt.inspect

    txt.gsub!(/^/, "#{Line.indent}#   ")
    Line.<< "\n#{txt}", :dont_move=>1

    nil
  end

  # !... lines (at left margin)
  Xiki.def(/^! /) do

    code =
      if Line =~ /^ *@/   # @!..., so just use one line
        Line[/! .+/]
      else   # !..., so use multiple lines
        Tree.siblings.select{|o| o =~ /^! /}.join("\n")
      end

    code.gsub! /^@?! /, ''

    # Calculate where we are in relation to the parent, for stack trace
    line_number = View.line
    lines_above = Tree.siblings(:before=>1).select{|o| o =~ /^! /}
    line_number -= lines_above.length

    returned, out, exception = Code.eval code, View.file, line_number, :pretty_exception=>1
    next exception if exception
    returned ||= out   # Use output if nothing returned
    returned = returned.to_s if returned

    next if returned.blank?
    Tree.quote returned
  end


  # Various lines that mean run as ruby
  # p ...
  # puts ...
  # etc.
  Xiki.def(/^(a|Ol|Ol\.a) /) do |line|
    line.sub! /^a /, "Ol.a "
    CodeTree.run line, :quote=>1
    "@flash/"
  end

  Xiki.def(/^(p|y|pp|puts) /) do |line|
    CodeTree.run line, :quote=>1
    nil
  end

  Xiki.def(/^ap /) do |line|
    CodeTree.run line, :quote=>1
  end

  Xiki.def(/^a /) do |line|   # "awesome out"
    line.sub! /^a /, "Ol.a "
    CodeTree.run line, :quote=>1
    "@flash/"
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
    path.sub! /^xiki/, 'http'
    path.gsub! ' ', '+'
    RestTree.request "GET", path
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


  Xiki.def(%r"^#(\w*)") do |path, options|
    next Xiki["ids/"].gsub(/^\+/, "<<") if path == "#"

    Xiki["ids/#{path}"]
  end


  Xiki.def(%r"^\.(\w*)") do |path, options|
    next Xiki["css/list/"].gsub(/^\+/, "<<") if path == "."

    Xiki["css/list/#{path}"]
  end

  # Probably make sure this one is the last!...

  # Paths in stack traces, etc
  #   Xiki.def(%r"/from (/.+):(\d+):") do |path, options|

  # Matches lines from el4r log like:
  # 2013-05-28 18:03:14 -0700:Error: uninitialized...
  #   from /Users/craig/.rvm/gems/ruby-1.9.3-p327@xiki/gems/trogdoro-el4r-1.0.7/bin/el4r-instance:847:in `instance_eval'

  Xiki.def(%r"^20.+/from (/.+):(\d+):") do |path, options|

    match = options[:expanders][0][:match]
    file, line = match[1..2]

    View.open file
    View.line = line
    nil

  end
end
