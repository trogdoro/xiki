# /, so show example...

return %`
  | # Put some sinatra code here and expand to try it out
  | get "/" do
  |   "Hi World!"
  | end
  | # url: /
` if args == []

# /code/~, so handle tilde options...

return "* run without browser" if task == []

without_browser = task == ["run without browser"]


# /code, so run it as sinatra app...

# Create file

txt = args[0]

if without_browser
  url = txt[/# url: (.+)/, 1] rescue "/"

  require_file = %`require "sinatra/base"`
  txt = "
    #{require_file}

    class"+" "+%`SinatraOo < Sinatra::Base

    # Not working :(
    # Show plaintext errors instead of sinatra's super-long default html error view
    set :show_exceptions, false
    error do
      err = env['sinatra.error']
      "Error: what?"
    end

  `.unindent+"\n\n"+txt+"\n\n"+"
    end
    puts SinatraOo.call(
      'REQUEST_METHOD' => 'GET',
      'PATH_INFO' => '#{url}',
      'rack.input' => StringIO.new
    )[2][0]
  ".unindent

  return Tree.quote Shell.command("ruby", :stdin=>txt)
end

# Add on require
require_file = txt =~ / < Sinatra::Base/ ? 'sinatra/base' : 'sinatra'
require_file = %`require "#{require_file}"`
txt = "#{require_file}\n\n#{txt}"

# Add on port, if it's an OO sinatra app
port = 4500 + rand(500)
txt.sub! /< Sinatra::Base\n/, "\\0  set :port, #{port}\n"

dir = "/tmp/sinatra"
FileUtils.mkdir_p dir
File.open("#{dir}/sinatra.rb", "w") { |f| f << txt }

# Kill any old server buffers from previous runs...

Buffers.list.map do |b|
  name = $el.buffer_name(b)
  next if name !~ /^console \/tmp\/sinatra\//
  # Kill sub-process of shell
  $el.interrupt_process(b, true) rescue nil
  Buffers.delete b
end

# Start up...

# Shell.async "ruby sinatra.rb -p #{port}", :dir=>dir, :dont_leave_bar=>1
Shell.async "ruby sinatra.rb -p #{port}", :dir=>dir, :dont_leave_bar=>1, :buffer=>"sinatra:#{port}"
ControlTab.go   # Switch back so server view isn't shown

sleep 0.2

url = txt[/# url: (.+)/, 1]

Browser.url "http://localhost:#{port}#{url}", :os_open=>1

""
