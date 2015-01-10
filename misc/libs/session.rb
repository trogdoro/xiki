# From the "session" gem.
# Copyright (c) 2014 Ara T. Howard
#
# License:
# same as Ruby's
# http://www.ruby-lang.org/en/LICENSE.txt

require 'open3'
require 'tmpdir'
require 'thread'
require 'yaml'
require 'tempfile'

module Session
  VERSION = '3.2.0'
  def self.version() VERSION end

  def Session.description
    'persistent connections with external programs like bash'
  end

  @track_history = ENV['SESSION_HISTORY'] || ENV['SESSION_TRACK_HISTORY']
  @use_spawn     = ENV['SESSION_USE_SPAWN']
  @use_open3     = ENV['SESSION_USE_OPEN3']
  @debug         = ENV['SESSION_DEBUG']

  class << self
    attr :track_history, true
    attr :use_spawn, true
    attr :use_open3, true
    attr :debug, true
    def new(*a, &b)
      Sh::new(*a, &b)
    end
    alias [] new
  end

  class PipeError < StandardError; end
  class ExecutionError < StandardError; end

  class History
    def initialize; @a = []; end
    def method_missing(m,*a,&b); @a.send(m,*a,&b); end
    def to_yaml(*a,&b); @a.to_yaml(*a,&b); end
    alias to_s to_yaml 
    alias to_str to_yaml 
  end # class History
  class Command
    class << self
      def cmdno; @cmdno ||= 0; end
      def cmdno= n; @cmdno = n; end
    end

    # attributes
    attr :cmd
    attr :cmdno
    attr :out,true
    attr :err,true
    attr :cid
    attr :begin_out
    attr :end_out
    attr :begin_out_pat
    attr :end_out_pat
    attr :begin_err
    attr :end_err
    attr :begin_err_pat
    attr :end_err_pat

    def initialize(command)
      @cmd = command.to_s
      @cmdno = self.class.cmdno
      self.class.cmdno += 1
      @err = ''
      @out = ''
      @cid = "%d_%d_%d" % [$$, cmdno, rand(Time.now.usec)]
      @begin_out = "__CMD_OUT_%s_BEGIN__" % cid
      @end_out = "__CMD_OUT_%s_END__" % cid
      @begin_out_pat = %r/#{ Regexp.escape(@begin_out) }/
      @end_out_pat = %r/#{ Regexp.escape(@end_out) }/
      @begin_err = "__CMD_ERR_%s_BEGIN__" % cid
      @end_err = "__CMD_ERR_%s_END__" % cid
      @begin_err_pat = %r/#{ Regexp.escape(@begin_err) }/
      @end_err_pat = %r/#{ Regexp.escape(@end_err) }/
    end
    def to_hash
      %w(cmdno cmd out err cid).inject({}){|h,k| h.update k => send(k) }
    end
    def to_yaml(*a,&b)
      to_hash.to_yaml(*a,&b)
    end
    alias to_s to_yaml 
    alias to_str to_yaml 
  end # class Command
  class AbstractSession 

  # class methods
    class << self
      def default_prog
        return @default_prog if defined? @default_prog and @default_prog
        if defined? self::DEFAULT_PROG
          return @default_prog = self::DEFAULT_PROG 
        else
          @default_prog = ENV["SESSION_#{ self }_PROG"]
        end
        nil
      end
      def default_prog= prog
        @default_prog = prog 
      end
      attr :track_history, true
      attr :use_spawn, true
      attr :use_open3, true
      attr :debug, true
      def init
        @track_history = nil
        @use_spawn = nil
        @use_open3 = nil
        @debug = nil
      end
      alias [] new
    end

  # class init
    init

  # attributes
    attr :opts
    attr :prog
    attr :stdin
    alias i stdin
    attr :stdout
    alias o stdout
    attr :stderr
    alias e stderr
    attr :history
    attr :track_history
    attr :outproc, true
    attr :errproc, true
    attr :use_spawn
    attr :use_open3
    attr :debug, true
    alias debug? debug
    attr :threads

  # instance methods
    def initialize(*args)
      @opts = hashify(*args)

      @prog = getopt('prog', opts, getopt('program', opts, self.class::default_prog))

      raise(ArgumentError, "no program specified") unless @prog

      @track_history = nil
      @track_history = Session::track_history unless Session::track_history.nil?
      @track_history = self.class::track_history unless self.class::track_history.nil?
      @track_history = getopt('history', opts) if hasopt('history', opts) 
      @track_history = getopt('track_history', opts) if hasopt('track_history', opts) 

      @use_spawn = nil
      @use_spawn = Session::use_spawn unless Session::use_spawn.nil?
      @use_spawn = self.class::use_spawn unless self.class::use_spawn.nil?
      @use_spawn = getopt('use_spawn', opts) if hasopt('use_spawn', opts)

      if defined? JRUBY_VERSION
        @use_open3 = true
      else
        @use_open3 = nil
        @use_open3 = Session::use_open3 unless Session::use_open3.nil?
        @use_open3 = self.class::use_open3 unless self.class::use_open3.nil?
        @use_open3 = getopt('use_open3', opts) if hasopt('use_open3', opts)
      end

      @debug = nil
      @debug = Session::debug unless Session::debug.nil?
      @debug = self.class::debug unless self.class::debug.nil?
      @debug = getopt('debug', opts) if hasopt('debug', opts) 

      @history = nil
      @history = History::new if @track_history 

      @outproc = nil
      @errproc = nil

      @stdin, @stdout, @stderr =
        if @use_spawn
          Spawn::spawn @prog
        elsif @use_open3
          Open3::popen3 @prog
        else
          __popen3 @prog
        end

      @threads = []

      clear

      if block_given?
        ret = nil
        begin
          ret = yield self
        ensure
          self.close!
        end
        return ret
      end

      return self
    end
    def getopt opt, hash, default = nil
      key = opt
      return hash[key] if hash.has_key? key
      key = "#{ key }"
      return hash[key] if hash.has_key? key
      key = key.intern
      return hash[key] if hash.has_key? key
      return default
    end
    def hasopt opt, hash
      key = opt
      return key if hash.has_key? key
      key = "#{ key }"
      return key if hash.has_key? key
      key = key.intern
      return key if hash.has_key? key
      return false 
    end
    def __popen3(*cmd)
      pw = IO::pipe   # pipe[0] for read, pipe[1] for write
      pr = IO::pipe
      pe = IO::pipe

      pid =
        __fork{
          # child
          pw[1].close
          STDIN.reopen(pw[0])
          pw[0].close

          pr[0].close
          STDOUT.reopen(pr[1])
          pr[1].close

          pe[0].close
          STDERR.reopen(pe[1])
          pe[1].close

          exec(*cmd)
        }

      Process::detach pid   # avoid zombies

      pw[0].close
      pr[1].close
      pe[1].close
      pi = [pw[1], pr[0], pe[0]]
      pw[1].sync = true
      if defined? yield
        begin
          return yield(*pi)
        ensure
          pi.each{|p| p.close unless p.closed?}
        end
      end
      pi
    end
    def __fork(*a, &b)
      verbose = $VERBOSE
      begin
        $VERBOSE = nil 
        Kernel::fork(*a, &b)
      ensure
        $VERBOSE = verbose
      end
    end

  # abstract methods
    def clear
      raise NotImplementedError
    end
    alias flush clear
    def path 
      raise NotImplementedError
    end
    def path= 
      raise NotImplementedError
    end
    def send_command cmd
      raise NotImplementedError
    end

  # concrete methods
    def track_history= bool
      @history ||= History::new
      @track_history = bool
    end
    def ready?
      (stdin and stdout and stderr) and
      (IO === stdin and IO === stdout and IO === stderr) and
      (not (stdin.closed? or stdout.closed? or stderr.closed?))
    end
    def close!
      [stdin, stdout, stderr].each{|pipe| pipe.close}
      stdin, stdout, stderr = nil, nil, nil
      true
    end
    alias close close!
    def hashify(*a)
      a.inject({}){|o,h| o.update(h)}
    end
    private :hashify
    def execute(command, redirects = {})
      $session_command = command if @debug

      raise(PipeError, command) unless ready? 

    # clear buffers
      clear

    # setup redirects
      rerr = redirects[:e] || redirects[:err] || redirects[:stderr] || 
             redirects['stderr'] || redirects['e'] || redirects['err'] ||
             redirects[2] || redirects['2']

      rout = redirects[:o] || redirects[:out] || redirects[:stdout] || 
             redirects['stdout'] || redirects['o'] || redirects['out'] ||
             redirects[1] || redirects['1']

    # create cmd object and add to history
      cmd = Command::new command.to_s

    # store cmd if tracking history
      history << cmd if track_history

    # mutex for accessing shared data
      mutex = Mutex::new

    # io data for stderr and stdout 
      err = {
        :io        => stderr,
        :cmd       => cmd.err,
        :name      => 'stderr',
        :begin     => false,
        :end       => false,
        :begin_pat => cmd.begin_err_pat,
        :end_pat   => cmd.end_err_pat,
        :redirect  => rerr,
        :proc      => errproc,
        :yield     => lambda{|buf| yield(nil, buf)},
        :mutex     => mutex,
      }
      out = {
        :io        => stdout,
        :cmd       => cmd.out,
        :name      => 'stdout',
        :begin     => false,
        :end       => false,
        :begin_pat => cmd.begin_out_pat,
        :end_pat   => cmd.end_out_pat,
        :redirect  => rout,
        :proc      => outproc,
        :yield     => lambda{|buf| yield(buf, nil)},
        :mutex     => mutex,
      }

    begin
      # send command in the background so we can begin processing output
      # immediately - thanks to tanaka akira for this suggestion
        threads << Thread::new { send_command cmd }

      # init 
        main       = Thread::current
        exceptions = []

      # fire off reader threads
        [err, out].each do |iodat|
          threads <<
            Thread::new(iodat, main) do |iodat, main|

              loop do
                main.raise(PipeError, command) unless ready? 
                main.raise ExecutionError, iodat[:name] if iodat[:end] and not iodat[:begin]

                break if iodat[:end] or iodat[:io].eof?

                line = iodat[:io].gets

                # In case their are weird chars, this will avoid a "invalid byte sequence in US-ASCII" error
                line.force_encoding("binary") if line.respond_to? :force_encoding

                buf = nil

                case line
                  when iodat[:end_pat]
                    iodat[:end] = true
                  # handle the special case of non-newline terminated output
                    if((m = %r/(.+)__CMD/o.match(line)) and (pre = m[1]))
                      buf = pre
                    end
                  when iodat[:begin_pat]
                    iodat[:begin] = true
                  else
                    next unless iodat[:begin] and not iodat[:end] # ignore chaff
                    buf = line
                end

                if buf
                  iodat[:mutex].synchronize do
                    iodat[:cmd] << buf
                    iodat[:redirect] << buf if iodat[:redirect]
                    iodat[:proc].call buf  if iodat[:proc]
                    iodat[:yield].call buf  if block_given?
                  end
                end
              end

              true
          end
        end
      ensure
      # reap all threads - accumulating and rethrowing any exceptions
        begin
          while((t = threads.shift))
            t.join
            raise ExecutionError, 'iodat thread failure' unless t.value
          end
        rescue => e
          exceptions << e
          retry unless threads.empty?
        ensure
          unless exceptions.empty?
            meta_message = '<' << exceptions.map{|e| "#{ e.message } - (#{ e.class })"}.join('|') << '>'
            meta_backtrace = exceptions.map{|e| e.backtrace}.flatten
            raise ExecutionError, meta_message, meta_backtrace 
          end
        end
      end

    # this should only happen if eof was reached before end pat
      [err, out].each do |iodat|
        raise ExecutionError, iodat[:name] unless iodat[:begin] and iodat[:end]
      end


    # get the exit status
      get_status if respond_to? :get_status

      out = err = iodat = nil

      return [cmd.out, cmd.err]
    end
  end # class AbstractSession
  class Sh < AbstractSession
    DEFAULT_PROG    = 'sh'
    ECHO            = 'echo'

    attr :status
    alias exit_status status
    alias exitstatus status

    def clear
      stdin.puts "#{ ECHO } __clear__ 1>&2"
      stdin.puts "#{ ECHO } __clear__"
      stdin.flush
      while((line = stderr.gets) and line !~ %r/__clear__/o); end
      while((line = stdout.gets) and line !~ %r/__clear__/o); end
      self
    end
    def send_command cmd
      stdin.printf "%s '%s' 1>&2\n", ECHO, cmd.begin_err
      stdin.printf "%s '%s' \n", ECHO, cmd.begin_out
 
      stdin.printf "%s\n", cmd.cmd
      stdin.printf "export __exit_status__=$?\n"

      stdin.printf "%s '%s' 1>&2\n", ECHO, cmd.end_err
      stdin.printf "%s '%s' \n", ECHO, cmd.end_out
 
      stdin.flush
    end
    def get_status
      @status = get_var '__exit_status__' 
      unless @status =~ /^\s*\d+\s*$/o
        raise ExecutionError, "could not determine exit status from <#{ @status.inspect }>"
      end

      @status = Integer @status
    end
    def set_var name, value
      stdin.puts "export #{ name }=#{ value }"
      stdin.flush
    end
    def get_var name
      stdin.puts "#{ ECHO } \"#{ name }=${#{ name }}\""
      stdin.flush

      var = nil
      while((line = stdout.gets))
        m = %r/#{ name }\s*=\s*(.*)/.match line
        if m
          var = m[1] 
          raise ExecutionError, "could not determine <#{ name }> from <#{ line.inspect }>" unless var
          break
        end
      end

      var
    end
    def path 
      var = get_var 'PATH'
      var.strip.split %r/:/o
    end
    def path= arg 
      case arg
        when Array
          arg = arg.join ':'
        else
          arg = arg.to_s.strip
      end

      set_var 'PATH', "'#{ arg }'"
      self.path
    end
    def execute(command, redirects = {}, &block)
    # setup redirect on stdin
      rin = redirects[:i] || redirects[:in] || redirects[:stdin] || 
             redirects['stdin'] || redirects['i'] || redirects['in'] ||
             redirects[0] || redirects['0']

      if rin
        tmp = 
          begin
            Tempfile::new rand.to_s
          rescue
            Tempfile::new rand.to_s
          end

        begin
          tmp.write(
            if rin.respond_to? 'read'
              rin.read
            elsif rin.respond_to? 'to_s'
              rin.to_s
            else
              rin
            end
          )
          tmp.flush
          command = "{ #{ command } ;} < #{ tmp.path }"
          #puts command
          super(command, redirects, &block)
        ensure
          tmp.close! if tmp 
        end

      else
        super
      end
    end
  end # class Sh
  class Bash < Sh
    DEFAULT_PROG = 'bash'
    class Login < Bash
      DEFAULT_PROG = 'bash --login'
    end
  end # class Bash
  class Shell < Bash; end
  # IDL => interactive data language - see http://www.rsinc.com/
  class IDL < AbstractSession
    class LicenseManagerError < StandardError; end
    DEFAULT_PROG = 'idl'
    MAX_TRIES = 32 
    def initialize(*args)
      tries = 0 
      ret = nil
      begin
        ret = super
      rescue LicenseManagerError => e
        tries += 1 
        if tries < MAX_TRIES
          sleep 1
          retry
        else
          raise LicenseManagerError, "<#{ MAX_TRIES }> attempts <#{ e.message }>"
        end
      end
      ret
    end
    def clear
      stdin.puts "retall"
      stdin.puts "printf, -2, '__clear__'"
      stdin.puts "printf, -1, '__clear__'"
      stdin.flush
      while((line = stderr.gets) and line !~ %r/__clear__/o)
        raise LicenseManagerError, line if line =~ %r/license\s*manager/io
      end
      while((line = stdout.gets) and line !~ %r/__clear__/o)
        raise LicenseManagerError, line if line =~ %r/license\s*manager/io
      end
      self
    end
    def send_command cmd
      stdin.printf "printf, -2, '%s'\n", cmd.begin_err
      stdin.printf "printf, -1, '%s'\n", cmd.begin_out

      stdin.printf "%s\n", cmd.cmd
      stdin.printf "retall\n"

      stdin.printf "printf, -2, '%s'\n", cmd.end_err
      stdin.printf "printf, -1, '%s'\n", cmd.end_out
      stdin.flush
    end
    def path 
      stdout, stderr = execute "print, !path"
      stdout.strip.split %r/:/o
    end
    def path= arg 
      case arg
        when Array
          arg = arg.join ':'
        else
          arg = arg.to_s.strip
      end
      stdout, stderr = execute "!path='#{ arg }'"

      self.path
    end
  end # class IDL
  module Spawn
    class << self
      def spawn command
        ipath = tmpfifo
        opath = tmpfifo
        epath = tmpfifo

        cmd = "#{ command } < #{ ipath } 1> #{ opath } 2> #{ epath } &"
        system cmd 

        i = open ipath, 'w'
        o = open opath, 'r'
        e = open epath, 'r'

        [i,o,e]
      end
      def tmpfifo
        path = nil
        42.times do |i|
          tpath = File::join(Dir::tmpdir, "#{ $$ }.#{ rand }.#{ i }")
          v = $VERBOSE
          begin
            $VERBOSE = nil
            system "mkfifo #{ tpath }"
          ensure
            $VERBOSE = v 
          end
          next unless $? == 0
          path = tpath
          at_exit{ File::unlink(path) rescue STDERR.puts("rm <#{ path }> failed") }
          break
        end
        raise "could not generate tmpfifo" unless path
        path
      end
    end
  end # module Spawn
end # module Session
