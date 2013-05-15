require 'rubygems'
require 'daemons'

pwd = Dir.pwd
result = Daemons.run_proc('xiki_web', {:dir_mode=>:normal, :dir=>"/tmp/", :log_output=>true}) do
  Dir.chdir(pwd)
  exec "ruby /projects/xiki/etc/www/sinatra_server.rb"
end

result
