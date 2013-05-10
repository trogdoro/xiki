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
