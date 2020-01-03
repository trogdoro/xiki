# This script is auto-generated. DON'T EDIT!!!
#!/usr/bin/env ruby

#### print
class Object
  # puts MSG = self.inspect
  # for debug.
  def pr(msg=nil)
    if msg then
      print "#{msg} = "
    end

    display
    print "\n"
    self
  end

  # pr when $VERBOSE
  def vpr(msg=nil)
    if $VERBOSE then
      self.pr msg
    else
      self
    end
  end

  # eval and print
  def ev(str)
    puts "#{str} = #{eval str}"
  end
end

#### array
class Array
  # Make random value array. length==n
  def Array.rand(n, r=0)
    (1 .. n).collect{super(r)}
  end

  # join ' '
  def js
    join ' '
  end
end

#### vrequire
# verbose require
def vrequire(f)
  puts "loading " + f + "..."
  require f
  puts "loading " + f + "...done"
end

#### vsystem
# verbose system
def vsystem(cmd)
  puts cmd
  system cmd
end

#### time
class Time
  # Simple benchmark function.
  # Time.time(start_msg) { ... }
  def Time.time(smsg="", emsg="%s seconds")
    s=Time.now
    print smsg
    puts if smsg != ""
    yield
    sec=Time.now - s
    printf emsg, sec
    puts ""
  end
end

#### nonzero
class Numeric
  # 1 : self >=0  / -1 : self < 0
  def sign
    if self >= 0 then 1 else -1 end
  end

  # If self is negative, returns 0
  def nonzero
    [self, 0].max
  end
end


class Vector
  # If self is negative, returns 0
  def nonzero
    map{|x| x.nonzero}
  end
end

class Matrix
  # If self is negative, returns 0
  def nonzero
    map{|x| x.nonzero}
  end
end

#### sigma
module Math
  # Math.sigma(b, e) { expr }
  def sigma(b,e)
    if b > e then
      0
    else
      s=0
      b.upto(e) do |i|
        s += yield(i) || 0
      end
      s
    end
  end
  alias sum sigma
  module_function :sigma
end

#### nonempty
class Object
  # Is self non-nil and not-empty.
  def nonempty?
    if self and !self.empty?
      self
    end
  end
end


#### tempfile
require 'pathname'
require 'tempfile'
require 'tmpdir'
class << Tempfile
  # Create a temporary file whose contents is CONTENT.
  # Returns the file's path.
  def path(content, dir=Dir.tmpdir)
    x = Tempfile.open("content", dir)
    x.write content
    x.close
    x.open
    x.path
  end

  # Similar to Tempfile.path. But returns Pathname object.
  def pathname(content, dir=Dir.tmpdir)
    Pathname.new(path(content, dir=Dir.tmpdir))
  end
end

#### untar
module Untar
  # Returns a command to uncompress an archive FILE.
  def untar_command(file)
    f = file
    case f
    when /\.tar\.gz$/, /\.tgz$/
      "(tar xzvf #{f} || tar xvf #{f})"
    when /\.tar\.bz2$/
      "(tar xjvf #{f} || tar xvf #{f})"
    when /\.tar$/, /\.gz$/
      "tar xf #{f}"
    when /\.zip$/
      "unzip #{f}"
    when /\.lzh$/
      "lha x #{f}"
    when /\.afz$/
      "afio -ivZ #{f}"
    when /\.rar$/i
      "unrar %s"
    when /\.sit$/i
      "ln -s %s tmp.sit; unstuff tmp.sit"
    else
      nil
    end
  end
end

#### getopts
def _getopts_sub(argv, single_opts, *long_opts)
  require 'optparse'
  opt = OptionParser.new

  (single_opts || "").split(//).each do |single_opt|
    opt.on("-#{single_opt}"){ eval "$OPT_#{single_opt}=true" }
  end

  long_opts.each do |long_opt|
    have_arg_p = (long_opt[-1,1] == ':')
    long_opt.chomp!(':')
    block = lambda{|x| eval "$OPT_#{long_opt}=x"}
    if have_arg_p
      if long_opt.length == 1   # -x arg
        opt.on("-#{long_opt} [ARG]",&block)
      else                    # --long arg
        opt.on("--#{long_opt}=[ARG]",&block)
      end
    else                        # --long
      opt.on("--#{long_opt}"){ eval "$OPT_#{long_opt}=true"}
    end
  end

  opt.parse! argv
end

# getopts compatibility layer using optparse.rb
def getopts(single_opts, *long_opts)
  _getopts_sub ARGV, single_opts, *long_opts
end

#### run config
# run config
#  c = RunConfig.new("test")
#  c.run_user_config
#  c.run_local_config
#
class RunConfig

  def initialize(name)
    @name = name
  end

  def run_user_config
    run_config [File.expand_path("~/.#{@name}rc")] if ENV.key?("HOME")
  end

  def run_local_config
    rcs = []
    rcs.push ".#{@name}rc"
    rcs.push "#{@name}.rc"
    rcs.push "_#{@name}rc"
    rcs.push "$#{@name}rc"
    rcs.push "#{@name}rc"
    run_config rcs
  end

  private
  def run_config(rcs)
    catch(:EXIT) do
      for rc in rcs
        begin
          load rc
          throw :EXIT
        rescue LoadError, Errno::ENOENT
        rescue
          print "load error: #{rc}\n"
          print $!.class, ": ", $!, "\n"
          for err in $@[0, $@.size - 2]
            print "\t", err, "\n"
          end
          throw :EXIT
        end
      end
    end
  end
end

#### set_attr
class Object
  # Defines a singleton attribute. for testing purpose.
  def set_attr(ivar_name, init_value)
    eval("class << self; attr_accessor :#{ivar_name} end")
    self.instance_variable_set("@#{ivar_name}", init_value)
  end
end

#### quote
class String
  def quote(q="'")
    %Q[#{q}#{self}#{q}]
  end

  # This function is different from dump.
  def dquote
    quote('"')
  end
end


#### assert-file
# Example1:
#     es = AssertFile.new("testdata/shorten-test.e")
#     el = AssertFile.new("testdata/shorten-test.el")
#     system "shorten.rb -l #{el} -e #{es}  testdata/shorten-test-input.e"
#     assert_file(es)
#     assert_file(el)
#
# Example2:
#     assert_file(:expected=>expected, :actual=>actual, :no_remove=>true)
#
# Example3:
#     AssertFile.transaction(expected) {|asf|
#       system "foo input > #{asf}"
#     }
class AssertFile
  require 'fileutils'
  @@basedir = nil
  def self.basedir=(basedir)
    @@basedir = basedir
  end

  def self.transaction(*args, &block)
    if block_given?
      testcase = eval("self", block)
      assert_files = args.map{|x| new(x) }

      if @@basedir
        Dir.chdir(@@basedir) { yield assert_files }
      else
        yield(assert_files)
      end
      assert_files.each{|asf| testcase.assert_file(asf)}
    else
      raise ArgumentError, "must have block"
    end
  end




  # new("expected_filename")
  # new(:expeced=>"expected_filename", :actual=>"actual_filename")
  # new(:expeced=>"expected_filename", :actual=>"actual_filename", :diff=>"diff")

  def initialize(arg)
    require 'test/unit'

    case arg
    when String               # expected
      @expected = arg
      @actual = arg+".actual"
      @diff = arg+".diff"
    when Hash
      @basedir = arg[:basedir]
      @expected = arg[:expected]
      @no_remove = arg[:no_remove]

      @actual = arg[:actual] || (@expected+".actual")
      @diff = arg[:diff] || (@expected+".diff")
    else
      raise TypeError, "AssertFile.new: must be String or Hash."
    end
    @basedir ||= @@basedir
    FileUtils.mkdir_p @basedir if @basedir
    @expected = pathconv(@expected)
    @actual = pathconv(@actual)
    @diff = pathconv(@diff)
  end
  attr_accessor :expected, :actual, :diff, :no_remove

  def pathconv(path)
    if @basedir
      File.expand_path(path, @basedir)
    else
      path
    end
  end


  def unlink_diff
    File.unlink(diff) if File.exist?(diff)
  end

  def make_diff
    system "diff -u #{expected} #{actual} | tee #{diff}"
  end

  def to_s
    actual
  end
  alias :to_str :to_s
end

module Test
  module Unit

    # Use at Test::Unit::Assertions::AssertionMessage#convert
    class System
      def initialize(cmd)
        @cmd = cmd
      end

      def inspect
        `#{@cmd}`.to_s
      end
    end

    module Assertions
      def assert_file(assert_file, message=nil)
        AssertFile === assert_file  or assert_file = AssertFile.new(assert_file)
        $>.sync = true
        assert_file.unlink_diff
        diff = System.new("diff -u #{assert_file.expected} #{assert_file.actual} | tee #{assert_file.diff}")
        full_message = build_message(message, <<EOM, diff)
#{assert_file.expected} and #{assert_file.actual} differ!
# #{assert_file.expected} size=#{File.size(assert_file.expected)}
# #{assert_file.actual} size=#{File.size(assert_file.actual)}
# expected (view-fline #{assert_file.expected.dump})
# actual   (view-fline #{assert_file.actual.dump})
# ediff    (ediff      #{assert_file.expected.dump} #{assert_file.actual.dump})
?
EOM
        assert_block(full_message) {
          File.read(assert_file.expected) == File.read(assert_file.actual)
        }
        File.unlink assert_file.actual unless assert_file.no_remove
      end
    end
  end
end

#### AssertFile depend: assert-file
#### w3m
module W3MUtils
  module_function

  def remove_local_cgi_url(url)
    case url
    when nil
      nil
    when %r!^file:///cgi-bin/fline.cgi\?(.+):\d+$!
      "file://#{$1}"
    else
      url
    end
  end
end

#### W3MUtils depend: w3m
#### iftest
# Test mode.
# Returns a temporary value when unit-testing.
module IfTest
  # Set true when unit-testing.
  attr_accessor :test_mode
  alias :test_mode? :test_mode


  # Returns a temporary value when unit-testing.
  def if_test(test_value, non_test_value=nil, &block)
    if self.test_mode?
      test_value
    else
      if block
        block.call
      else
        non_test_value
      end
    end
  end
end

#### IfTest depend: iftest

#### tempdir
require 'tmpdir'
# Makes a temporary directory and executes a block and cleans the directory.
def with_temp_dir(dir=Dir.tmpdir+"/tmp_#{$$}")
  require 'fileutils'
  begin
    dir = Pathname.new(dir).expand_path
    dir.mkdir
    Dir.chdir(dir) { yield(dir) }
  ensure
    FileUtils.rm_rf(dir)
  end
end

#### textarea
class String

  def textarea_default_cols
    begin
      require 'curses'
      Curses.init_screen
      Curses.stdscr.maxx-3
    ensure
      Curses.close_screen
    end
  end
  private :textarea_default_cols

  # Makes a string textarea-ize.
  # COLS is adjusted to terminal by default.
  # String is HTML escaped when ESCAPE is true.
  def textarea_ize(cols=nil, rows=nil, escape=true)
    cols ||= textarea_default_cols
    rows = self.split(/\r?\n/).inject(0){|result, item| result + (item.length/cols+1)}+1 unless rows
    content = if escape
                require 'fastesc'
                self.html_escape
              else
                self
              end
    "<textarea rows=#{rows} cols=#{cols}>#{content}</textarea>"
  end

  # Same as textarea_ize. But the string is not escaped.
  # It is expected that the string is HTML.
  def textarea_ize_noconv(cols=nil, rows=nil)
    textarea_ize(cols, rows, false)
  end

end

#### redirect
class << IO
  # Redirect stdout to STDOUT and executes the block.
  def redirect(stdout)
    begin
      stdout_sv = STDOUT.dup
      STDOUT.reopen(stdout)
      yield
    ensure
      STDOUT.flush
      STDOUT.reopen(stdout_sv)
    end
  end
end

#### system_to_string depend:redirect
# Similar to `` [backquotes]. If multiple arguments are given, the
# second and subsequent arguments are passed as parameters to command
# with no shell expansion.
require 'tmpdir'
require 'fileutils'



# Commenting might cause problems!
# @@__system_to_string_count__ = 0



def system_to_string(*args)
  begin
    tmpf = File.join(Dir.tmpdir, "#{$$}-#{@@__system_to_string_count__}")
    @@__system_to_string_count__ += 1
    ret = nil
    open(tmpf,"w") do |f|
      IO.redirect(f) {
        system *args
      }
    end
    File.read(tmpf)
  ensure
    FileUtils.rm_f tmpf
  end
end

#### EmacsLisp depend: system_to_string
module EmacsLisp
  # Executes an EmacsLisp string by gnudoit.
  def elisp(lisp)
    system_to_string("gnudoit", lisp).chomp
  end

  # Converts a Ruby string to EmacsLisp string.
  # [imported from el4r]
  def dump_string(string)
    dumped = string.dup
    # \ -> \\
    dumped.gsub! %r"\\" do '\\\\' end
    # " -> \"
    dumped.gsub! %r'"' do '\\"' end
    # (zero byte) -> \0
    dumped.gsub! %r'\0' do "\\\0" end
    %Q'"#{dumped}"'
  end

end

#### flib
class Object
  # Same as File.read. But FILENAME is expanded.
  def readf(filename)
    File.read( File.expand_path(filename.to_s) )
  end

  # Write an object's string form. FILENAME is expanded.
  def writef(filename)
    open(File.expand_path(filename.to_s), "w"){|f| f.write(self.to_s)}
  end
end

#### notify_exit
def notify_exit
  # Notify when the program is exited.
  at_exit do
    bell_message "#$0 exited."
  end
end

#### region
class String
  # Scans a regexp once. Then cut matched part from string. Returns the matched part.
  def kill_region!(regexp)
    ret = ""
    sub!(regexp) {
      ret = $&
      ""
    }
    ret
  end
end

#### ext
class String
  # Returns a string which is replaced the filename's extension with NEWEXT.
  def ext(newext=nil)
    if newext
      newext[0,1] != '.' and newext="."+newext
      sub(/\.[^\.]+?$/, newext)
    else
      File.extname(self)
    end
  end

  # Returns a string which is stripped the filename's extension.
  def noext
    sub(/\.[^\.]+$/,'')
  end
end

#### StructWithType
class StructWithType < Struct

  # TODO: document
  def self.new(*args)

    keys = []
    types = []
    args.each_with_index do |x,i|
      if i%2 == 0
        keys << x
      else
        types << x
      end
    end

    unless keys.length > 0 &&
        types.length > 0   &&
        keys.length == types.length
      raise ArgumentError, "#{self}: args.length must be even"
    end


    klass = super(*keys)

    klass.instance_eval do
      @@__type_dic__ = {}
      @@__keys__ = keys
      keys.each_with_index{|k,i| @@__type_dic__[k] = types[i]}
    end

    klass
  end

  def initialize(*args)
    args.each_with_index do |x, i|
      args[i] = __convert__(@@__keys__[i], x)
    end

    class << self
      @@__keys__.each do |k|
        define_method("#{k}="){|v| self[k]=v}
      end
    end

    super *args
  end

  def __convert__(k,v)
    __send__(@@__type_dic__[k.to_sym],v)
  end
  private :__convert__

  def []=(k,v)
    v = __convert__(k,v)
    super(k,v)
  end


end

#### ep
class String
  # Expand tilde
  def ep
    case self
    when /^~/
      File.expand_path(self)
    else
      self
    end
  end
end


#### change_home
class File
  def self.change_home(dir)
    oldhome = ENV['HOME']
    begin
      ENV['HOME'] = dir
      yield(dir)
    ensure
      ENV['HOME'] = oldhome
    end
  end
end

#### mapf
module Enumerable
  #
  # "map function"
  #   enum.mapf(:x)
  # is short for
  #   enum.map { |elt| elt.x }
  #
  def mapf(message)
    self.map { |elt| elt.send(message) }
  end
end

#### build_hash
module Enumerable
  #
  # Like <tt>#map</tt>/<tt>#collect</tt>, but it generates a Hash.  The block
  # is expected to return two values: the key and the value for the new hash.
  #   numbers  = (1..3)
  #   squares  = numbers.build_hash { |n| [n, n*n] }   # 1=>1, 2=>4, 3=>9
  #   sq_roots = numbers.build_hash { |n| [n*n, n] }   # 1=>1, 4=>2, 9=>3
  #
  def build_hash
    result = {}
    self.each do |elt|
      key, value = yield elt
      result[key] = value
    end
    result
  end

end

#### map_with_index
module Enumerable
  #
  # Same as Enumerable#map, but the index is yielded as well.  See
  # Enumerable#each_with_index.
  #   puts files.map_with_index { |fn, idx| "#{idx}. #{fn}" }
  #   print "Please select a file (0-#{files.size}): "
  #
  def map_with_index
    result = []
    self.each_with_index do |elt, idx|
      result << yield(elt, idx)
    end
    result
  end
end



#### bug!
class ScriptBug < Exception; end

# Raises ScriptBug exception.
def bug!( message = 'must not happen' )
  raise ScriptBug, "\n[SCRIPT BUG] " + message
end

#### must
class Object

  # Assert: type === obj
  # ex. obj.must Fixnum, Float
  def must( *args )
    args.each {|c| return self if c === self }
    raise TypeError, "wrong arg type '#{self.class}' for required #{args.join('/')}"
  end

  # Assert: obj.respond_to? meth
  # ex. obj.must_have :read, :readlines
  # ex. obj.needed :read, :readlines
  def must_have( *args )
    args.each do |m|
      self.respond_to? m or
          raise ArgumentError, "receiver #{inspect} does not have '#{m}'"
    end
    self
  end

  alias needed must_have

  # Assert: self == obj
  def must_be( obj )
    self == obj or
            raise ArgumentError, "expected #{obj.inspect} but is #{inspect}"
    self
  end

  # Assert: self != nil
  def must_exist
    nil? and raise ArgumentError, 'receiver is wrongly nil'
  end

end



#### Contents
class GenericContents
  require 'forwardable'
  extend Forwardable
  methods = String.instance_methods(false) - %w[to_s to_str]
  def_delegators(:@to_s, *methods)
  attr :to_s
  alias :to_str :to_s
end

# str = FileContents.new(filename)
class FileContents < GenericContents
  def initialize(filename)
    @to_s = File.read(filename)
  end
end

# str = URIContents.new(uri)
class URIContents < GenericContents
  def initialize(uri)
    require 'open-uri'
    @to_s = URI(uri).read
  end
end


#### show_usage
# Prints the script's first comment block.
def show_usage(msg=nil)
  name = caller[-1].sub(/:\d+$/, '')
  $stderr.puts "\nError: #{msg}" if msg
  $stderr.puts
  File.open(name) do |f|
    while line = f.readline and line.sub!(/^# ?/, '')
      $stderr.puts line
    end
  end
  exit 1
end

#### UnTable
# Strip table-related tags in HTML
class UnTable
  # Strip table-related tags in SRC
  def untable!(src)
    src.gsub!(%r!</?(table)\b[^>]*>!i,'')
#    src.gsub!(%r!<t[hd]\b[^>]*>!i,'&nbsp;')
    src.gsub!(%r!<t[hd]\b[^>]*>!i,'')
    src.gsub!(%r!<tr[^>]*>!i,'<br>')
    src.gsub!(%r!</tr>!i, '')
    src
  end

  def untable(src)
    untable!(src.dup)
  end
end


#### system_safe
# mswin32 ruby's `system' workaround.
def system_safe(*x)
  begin
    system *x
  rescue
  end
end


#### unproc
class Object
  def unproc(*x)
    self
  end
end

class Proc
  def unproc(*x)
    call *x
  end
end

#### ConfigScript depend: set_attr, build_hash, unproc
require 'forwardable'
class ConfigScript
  extend Forwardable
  include Enumerable
  def initialize(arg)
    unless Hash === arg
      eval(readf(arg.to_s))
      arg = instance_variables.map{|iv|
        [iv[1..-1], instance_variable_get(iv)]
      }.build_hash{|kv| kv}
    end

    s_class = class << self; self end
    arg.each do |k,v|
      if Symbol === k
        arg.delete k
        k = k.to_s
        arg[k]=v
      end

      s_class.class_eval do
        define_method(k) {arg[k].unproc}
        define_method("#{k}=") {|v| arg[k]=v}
      end
    end
    @hash = arg
  end

  alias :[] :__send__
  def_delegators :@hash, :keys, :each

  def []=(k,v)
    @hash[k.to_s]=v
  end

  def method_missing(name, *args, &block)
    nil
  end
end


#### dump
class Numeric
  def dump() self end
end

#### __funcall
class Object
  # Call a method even if it is private.
  def __funcall(meth, *args, &block)
    m = method(meth)
    instance_eval { m.call(*args, &block) }
  end
end


#### scriptdir
class Dir
  def Dir.scriptdir
    File.dirname(File.expand_path(caller(1)[0].scan(/^(.+?):\d+:/).to_s))
  end
end

#### URLConv
module URLConv
  def relative2absolute(html, baseurl)
    # relativelink to absolute link
    html.gsub!(/(href|src)=['"]?\s*(.*?)\s*?['"]?(>|\s)/mi) do |x|
      begin
        uri = URI.parse($2)

        absolute_url = if uri.scheme.nil?
                         URI.join(baseurl, $2)
                       else
                         $2
                       end

        "#{$1}=\"#{absolute_url}\"#{$3}"
      rescue URI::InvalidURIError
        next
      end
    end
  end
end

#### END OF LIBRARY
# To add a new code (mylib-rb-add)

#### test
if __FILE__==$0

  class MylibCommand
    def initialize
      @lines = File.readlines($0)
    end

    def run
      meth = "do_#{ARGV[0]}"
      if respond_to?(meth)
        __send__ meth
      else
        do_list
      end
    end

    def do_list
      @lines.select{|line|
        line =~ /^ *(class|module|def|attr|attr_reader|attr_writer|attr_accessor) |^#### /
      }.display
    end

    def do_pieces
      @lines.inject([]){|result, line|
        if line =~ /^#### (.+?)/
          result + ["#{$1}\n"]
        else
          result
        end
      }.display
    end
    alias :do_piece :do_pieces
  end

  MylibCommand.new.run
end

