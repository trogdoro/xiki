"
  To run:
    !!merb -I merb_error_opener.rb -p 9637

  Put this in your .emacs first
    (server-start)
"

Merb::Router.prepare do |r|
  r.match('/').to(:controller => 'opener_flat', :action =>'index')
end

class OpenerFlat < Merb::Controller
  def index
    url, line = params[:url], params[:line]
    url.gsub!(/^file:\/\/\//, '/')

    command =
      %Q<emacsclient -n -e '(progn
        (el4r-ruby-eval "
          View.open \\"#{url}\\"
          View.to_line #{line}
          Color.colorize :e
          View.focus
          ")
        )'>

   command_output = `#{command}`

   "<script>history.back()</script>"
  end
end

Merb::Config.use { |c|
  c[:framework]           = {},
  c[:session_store]       = 'none',
  c[:exception_details]   = true,
  c[:reload_classes]      = true,
  c[:reload_time]         = 0.5
}

# Reload class when modified
Merb::BootLoader.before_app_loads do
  Merb.push_path(:application, Merb.root, File.basename(__FILE__))
end
