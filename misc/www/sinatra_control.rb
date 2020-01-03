require "/projects/xiki/lib/xiki/core/ol.rb"
Ol()

require 'rubygems'
require 'daemons'

# Temp

pwd = Dir.pwd
Ol "pwd", pwd
Ol RUBY_VERSION

result = Daemons.run_proc('xiki_web', {:dir_mode=>:normal, :dir=>"/tmp/", :log_output=>true}) do
  Dir.chdir(pwd)
  exec "ruby xiki_web_server.rb"
end

result
