gem 'net-ssh'
require 'net/ssh'

# Wraps around a local or remote irb or merb (and probably rails)
# console, passing commands to it and returning their stdout output.
class RubyConsole

  @@output = ""
  def self.output;  @@output;  end

  @session = @channel = nil

  CODE_SAMPLES = %q<
    # Send something to a local irb console
    RubyConsole.run("puts 'hey'")

    # Send something to a merb console
    RubyConsole.register(:foo, 'merb -m /projects/foo -i')  # Do this only once
    RubyConsole[:foo].run("y Account.first")
  >

  @@registered = {}

  def initialize console_command='irb', server=nil
    @console_command, @server = console_command, server
  end

  def connect
    timeout(25) do
      open_channel  # Connect
# TODO: try to indent this
      send_it %Q[conf.echo = false

# Temorary hack for nuby 1.8.4
$out_bufr = defined?(StringIO) ? StringIO.new : ''

module Kernel
  def puts *args
    args.each { |a|
      $out_bufr << (
        (a.class == Array) ?
          (a.join("\\n") + "\\n") :
          "\#{a}\\n"
      )
    }
    nil
  end
  def p *args
    args.each { |a| $out_bufr << "\#{a.inspect}\n" }
    nil
  end
end
]
    end
  end

  # Send output
  def send_it the_command
    time_stamp = "-eor#{Time.now.usec}-"
    the_command = "\
# Temorary hack for nuby 1.8.4
$out_bufr = defined?(StringIO) ? StringIO.new : ''
begin
#{the_command}
rescue Exception => e
  puts e.message
end
# Temorary hack for nuby 1.8.4
out = $out_bufr.respond_to?(:string) ? $out_bufr.string : $out_bufr
puts '#{time_stamp}'
$stdout.print out
"

    #the_command.gsub!(/$/, " # input!")
    begin
      @channel.send_data(the_command)
      @session.loop 10 do |session|
        # Return true, unless found output
        @@output !~ /^#{time_stamp}$/
      end
    rescue Exception => e
      puts "too slow! #{e.message}"
    end
  end

  def open_channel
    @session = if @server
      user, server, port = Remote.split_root @server
      Remote.new_connection(user, server, port)
    else
      Net::SSH.start('localhost', ENV['USER'] || ENV['USERNAME'])
    end
    @channel = @session.open_channel do |ch|
      ch.exec @console_command do |ch, success|
        raise "could not execute command" unless success
        # When console returns (prints) something
        ch.on_data do |c, data|
          # Use to debug problems / errors on server
          # Ol << "data: #{data.inspect}"
          RubyConsole.output << data
          #print data
        end

        # "on_extended_data" is called when the process writes something to stderr
        ch.on_extended_data do |c, type, data|
          print "error: #{data}"
        end
        ch.on_close { puts "done!" }
      end
    end

  end

  def run the_command
    connect unless @channel
    send_it the_command
    out = @@output
    @@output = ""

    # Remove up until beginning of output
    out.sub!(/.*^.*\$stdout\.print out\n/m, '')
    # Remove last line
    out.sub!(/^.?.?.?-eor\d+-\n.*/, '')

    out == "" ? "(no output)\n" : out
  end

  def console_command= to; @console_command = to; end
  def console_command; @console_command; end

  def self.run the_command
    self[:irb].run the_command
  end

  def self.[] key
    @@registered[key]
  end

  def self.register key, console_command='irb', server=nil
    @@registered[key] = RubyConsole.new(console_command, server)
  end

  def self.at key, the_command
    console = self[key]
    raise "No console has been defined for key '#{key}'." unless console
    console.run the_command
  end

  def session
    connect unless @channel
    @session
  end

end

RubyConsole.register(:irb, 'irb')
