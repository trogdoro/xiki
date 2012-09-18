require 'socket'
require 'timeout'
require 'xiki_process'

#
# The 'xiki' shell command uses this class to run xiki menus.
# It tells the xiki process to run menus.  It starts the xiki
# process if it's not running yet, and the process stays alive
# until the next time the shell command is run.
#
class XikiCommand

  @@dont_show_output = false

  # Called by the 'xiki' shell command
  def self.run

    xiki_dir = File.expand_path "#{File.dirname(__FILE__)}/../.."
    argv = ARGV

    if argv.empty?
      puts "#{self.usage}\n"
      @@dont_show_output = true
      argv = ['start']   # So it continues on and starts server
    elsif argv.length == 1 && ['status', 'stop', 'restart'].member?(argv[0])
      return self.ctrl argv[0]
    end

    flags, path = argv.partition{|o| o =~ /^-/}
    path = path.join ' '

    return self.emacs path if flags.member?("-e")   # If -p, just prompt user to type a menu name

    XikiProcess.start_daemon unless XikiProcess.running?

    attempts = 0
    begin
      attempts += 1
      TCPSocket.open('localhost', 22112) do |out|
        out.puts path

        timeout( 3 ) do
          response = out.gets
          response.strip!
          response.gsub! "\036", "\n"   # Escape linebreaks as 036 char (record separator)
          return if @@dont_show_output
          puts self.add_coloring response
        end
      end
    rescue Exception => e
      sleep 1
      retry if attempts < 10
      # Handle specific errno here
    end
  end

  def self.usage
    txt = %`
      > Summary
      The 'xiki' shell command is mostly meant to be called by programs
      that want to interface with Xiki.  But it is sometimes useful for
      people to call it directly.  Example usages:

      $ xiki ip
      $ xiki docs/faq

      > Setting up your editor
      The most common way to use Xiki is from a text editor.  For
      example, from in a text editor, typing "tables" on any blank line
      and then double-clicking on it (or typing control-enter or
      command-enter) to browse and update your mysql database.

      See the README.markdown file in the Xiki dir for help setting up
      your editor. You can view it by typing this command or going to
      this url:

      $ xiki readme

      https://github.com/trogdoro/xiki

      > Service
      The 'xiki' shell command automatically runs a service in the
      background to keep things fast.

      % xiki status
      % xiki stop
      % xiki restart

      > Interfaces
      Xiki can be used from...
      - A text editor
      - The 'xiki' shell command
      - The http://xiki/ url in your browser (experimental)

      For more information type:

      $ xiki docs

      > Google Group and Twitter
      Join the google group or follow @xiki on twitter for help with
      installing and using, or just to chat or share your ideas:

      http://groups.google.com/group/xiki/
      http://twitter.com/xiki

      > Troubleshooting
      A couple commands to help you trouble-shoot:

      % bundle install
      % xiki readme

      `.unindent

    self.add_coloring txt
  end

  def self.ctrl action
    require 'daemons'
    xiki_dir = File.expand_path "#{File.dirname(__FILE__)}/../.."
    xiki_process = "#{xiki_dir}/etc/command/xiki_process.rb"
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


  def self.add_coloring txt
    return txt if ! STDOUT.tty?
    txt.gsub!(/.+/) do |line|
      case line
      when /^(>) (.+)/
        "#{self.heading_bracket $1} #{self.bold $2}"
      when /^(>>+) (.+)/
        "#{self.heading_bracket $1} #{self.path $2}"
      when /^ *https?:\/\/.+/
        "#{self.url $&}"
      when /^.+\/$/
        "#{self.path $&}"
      when /^( *- )(.*!)$/
        "#{self.bullet $1}#{self.exclamation $2}"
      when /^( *)(- )(.+: )(.+)/
        "#{$1}#{self.bullet $2}#{self.label $3}#{$4}"
      when /^( *)(- )(.+)/
        "#{$1}#{self.bullet $2}#{$3}"

      else
        line
      end
    end

    txt << "\n"   # Add extra linebreak, but only when in console
    txt
  end

  def self.colorize txt, color_code
    "\e[#{color_code}m#{txt}\e[0m"
  end
  def self.bullet txt
    self.colorize txt, "1;31"   # Red # colorize("1;91")   # Red
  end
  def self.label txt
    self.colorize txt, "1;90"   # Gray
  end
  def self.path txt
    colorize(txt, "1;90")
  end
  def self.url txt
    colorize(txt, "1;36")   # Cyan
  end
  def self.heading_bracket txt
    colorize(txt, "0;37")
  end
  def self.bold txt
    colorize(txt, "1")
  end
  def self.exclamation txt
    colorize(txt, "1;32")   # Green
  end

end
