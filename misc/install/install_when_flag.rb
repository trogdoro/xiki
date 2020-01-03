path = File.expand_path __FILE__
path = File.dirname path

require "#{path}/save_config.rb"

Xiki::SaveConfig.save_config "/xiki", "some shell key shortcut remappings"


