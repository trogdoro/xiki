require 'timeout'

#
# The 'xiki' shell command uses this class to run xiki menus.
# It tells the xiki process to run menus.  It starts the xiki
# process if it's not running yet, and the process stays alive
# until the next time the shell command is run.
#
class XikiCommand

  @@dont_show_output = false
  @@initial_request = nil
  def self.pop_initial_request
    tmp = @@initial_request
    @@initial_request = nil   # So it doesn't loop
    tmp
  end

  # Called by the 'xiki' shell command
  def self.run

    xiki_root = File.expand_path "#{File.dirname(__FILE__)}/../.."
    argv = ARGV

    if argv.empty?
      puts self.usage
      @@dont_show_output = true
      argv = ['start']   # So it continues on and starts server
    elsif argv.length == 1 && ['status', 'stop', 'restart'].member?(argv[0])
      return self.ctrl argv[0]
    end

    flags, path = argv.partition{|o| o =~ /^-/}
    path = path.join ' '

    return self.emacs path if flags.member?("-e")   # If -p, just prompt user to type a menu name

    wasnt_running = false

    begin
      `mkfifo -m 666 /tmp/xikirequest` if ! File.exists?("/tmp/xikirequest")   # Always create first, so they have to be pipes and can't be files
      `mkfifo -m 666 /tmp/xikiresponse` if ! File.exists?("/tmp/xikiresponse")

      # Try writing to pipe...

      open("/tmp/xikirequest", 'w+') do |out|
        out.puts path
        out.flush   # do this when we're done writing data
        out.close
      end

      # Try reading from pipe...

      response = self.get_response
      return response

    rescue Exception=>e

      # If error, remember we have to start the process...

      if e.is_a?(Timeout::Error) || (e.is_a?(Errno::ENOENT) && e.to_s =~ /\/tmp\/xiki/)
        # Seems like process hasn't been started, so keep going
        wasnt_running = true
      else
        Ol << "Unknown error when trying to call pipe!"
        Ol << "#{e.to_s}\n#{e.backtrace.join "\n"}"
        return
      end
    end

    process_succeeded = false

    if wasnt_running

      # Start the process...

      require 'daemons'

      begin

        # Shell out as async to start it instead - don't know fucking how

        pid_orig = Process.pid

        @@initial_request = path
        xiki_process = "#{xiki_root}/etc/command/xiki_process.rb"
        Daemons.run xiki_process, :ARGV=>['start'], :monitor=>false, :multiple=>false, :dir_mode=>:normal, :dir=>"/tmp/", :log_dir=>"/tmp/", :log_output=>true

        # Aparently this line never gets reached
        "- Started the process, I think."
      rescue SystemExit=>e
        raise SystemExit.new if pid_orig != Process.pid   # If new process that started, just exit

        process_succeeded = true

      rescue Exception=>e
        puts "- service couldn't start!"
      end
    end

    if process_succeeded
      puts self.get_response   # Get response first time
    end

    raise SystemExit.new
  end

  def self.get_response

    # Simulate timeout error if process not running
    process_running = `ps -e` =~ /xiki_process.rb/
    if ! process_running
      raise Timeout::Error
    end

    # TODO: what if menu takes longer than 3 seconds to run?

    # TODO: have process send ACK, so we can make timout short

    timeout(3) do
    #     timeout(0.5) do
    #     timeout(1.5) do
      open("/tmp/xikiresponse", "r+") do |response|

        # old TODO Try using select here, to see if there's data avaliable
        # old IO.select ["/tmp/xikiresponse"]

        response = response.gets   # will block if there's nothing in the pipe

        response.strip!
        response.gsub! "\036", "\n"   # Escape linebreaks as 036 char (record separator)
        response = "" if @@dont_show_output
        response
      end
    end
  end

  def self.usage
    %`
    > Summary
    This command runs xiki menus, which come from simple files
    found in ~/menus/.

    > Show all menus
    % xiki all

    > Examples
    Call 'ip' menu
    % xiki ip
    % xiki animals
    % xiki tables

    > Open 'ip' menu in emacs
    % xiki -e ip

    > Run under current dir
    % xiki -e @git

    > Service
    Xiki automatically runs a service in the backgroundto keep
    things fast.

    % xiki   # With no args, it starts the service.
    % xiki status
    % xiki stop
    % xiki restart
    `.unindent
  end

  def self.ctrl action
    require 'daemons'
    xiki_root = File.expand_path "#{File.dirname(__FILE__)}/../.."
    xiki_process = "#{xiki_root}/etc/command/xiki_process.rb"
    Daemons.run xiki_process, :ARGV=>[action], :dir_mode=>:normal, :dir=>"/tmp/", :log_dir=>"/tmp/", :log_output=>true
    ""
  end

  #
  # Tells emacs to open and display menu.
  #
  def self.emacs menu

    # Bring emacs to front

    `open "/Applications/Aquamacs Emacs.app"`

    ruby = %`Menu.external \\"#{menu}\\"`
    ruby << %`, :dir=>\\"#{Dir.pwd}\\"` if menu =~ /^@/

      command = %`emacsclient -n -e '(el4r-ruby-eval "#{ruby}")'`
    `#{command}`

    nil
  end

end

