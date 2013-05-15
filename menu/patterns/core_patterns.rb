load "#{Xiki.dir}menu/mysql/index.rb"   # Necessary since it's lazy loaded
Xiki::Menu::Mysql.def_patterns

# $... shell command lines when not under dir
Xiki.def(/^([$%&]) (.+)/) do |path, options|
  match = options[:expanders].find{|o| o[:match]}[:match]
  prompt, command = match[1..2]
  options[:command] = command

  Console.shell_command_per_prompt prompt, options
end

# Url's
Xiki.def(%r"^(http|file).?://") do |path, options|

  # Todo: make this work when no editor

  Launcher.append_log path

  prefix = Keys.prefix
  Keys.clear_prefix

  url = path[/(http|file).?:\/\/.+/]
  if prefix == "all"
    txt = RestTree.request("GET", url)
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
  Effects.blink(:what=>:line)
  nil
end


# !... lines (at left margin)
Xiki.def(/^! /) do
  code = Tree.siblings(:all=>1).select{|o| o =~ /^! /}.join("\n")
  code.gsub! /^! /, ''

  line_number = View.line

  lines_above = Tree.siblings(:before=>1).select{|o| o =~ /^! /}
  line_number -= lines_above.length


  returned, out, exception = Code.eval code, View.file, line_number
  returned ||= out   # Use output if nothing returned
  returned = returned.to_s if returned
  returned
end


# Various lines that mean run as ruby
# p ...
# puts ...
# etc.

Xiki.def(/^(p|y|pp|puts|Ol) /) do |line|
  CodeTree.run line, :quote=>1
end

Xiki.def(/^(ap) /) do |line|
  CodeTree.run line, :quote=>1
end

Xiki.def(/^print\(/) do |line|
  Javascript.launch
end

Xiki.def(/^pg /) do |line|
  line.sub! /^pg /, ''
  CodeTree.run "puts JSON.pretty_generate(#{line})"
end

