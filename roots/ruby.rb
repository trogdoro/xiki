module Xiki
  # Wraps @eval, because "ruby" looks more appropriate
  # in some cases, like:
  #
  # foo/docs/counting/
  #   @ruby/
  #     | p "123"
  class Ruby
    def self.menu *args

      options = yield

      # /, so show default message...

      if args == []
        return "
          | # Put some code here and expand to run it
          | a = 1
          | a + a
          "
      end

      if task = options[:task]
        return "~ run in shell" if task == []

        # ~ run in shell, so run in console...

        # return args[0].inspect

        File.open("/tmp/tmp.rb", "w") { |f| f << args[0] }

        # Shell.async "ruby standalone.rb", :dir=>"/tmp"
        DiffLog.quit_and_run "ruby tmp.rb", :dir=>"/tmp"
        return ""
      end

      options[:no_slash] = 1

      cursor = View.cursor
      Tree.to_parent
      line_number = View.line+1
      View.cursor = cursor

      code = args[-1]

      code.gsub!(/^: /, '') if code =~ /\A: .+\z/   # If single :... line, unquote

      txt, out, exception = Code.eval code, View.file, line_number, :pretty_exception=>1

      return exception if exception

      txt = txt.to_s rescue "couldn't parse result"

      txt = out || txt
      txt = Tree.quote txt if txt.any?   # Don't quote if blank line
      txt
    end
  end
end

#
# Old =ruby command:
#
#:s/xiki/commands/
#  - ruby/
#    - index.menu
#      : - .classes/
#      : - load path/
#      :   ! $LOAD_PATH.map{|o| "=#{o}/\n"}.join("")
#      : - version/
#      :   ! RUBY_VERSION
#    - index.rb
#      : module Xiki::Menu
#      :   class Ruby
#      :
#      :     def self.classes clazz=nil, method=nil
#      :       # /classes/, so show list of classes...
#      :
#      :       if clazz.nil?
#      :         result = []
#      :         ObjectSpace.each_object(Class) do |c|
#      :           name = c.to_s
#      :           next if name =~ /^#/
#      :           result << "- #{name}/\n"
#      :         end
#      :         return result.sort.join
#      :       end
#      :
#      :       # /classes/Class, so show methods...
#      :
#      :       if method.nil?
#      :         result = ""
#      :         result << Kernel.const_get(clazz).instance_methods(false).sort.
#      :           collect {|i| "- #{i}/" }.join("\n")
#      :         result << Kernel.const_get(clazz).methods(false).sort.
#      :           collect {|i| "- ::#{i}/" }.join("\n")
#      :         return result
#      :       end
#      :
#      :       # /classes/Class/method, so lookup method's doc
#      :
#      :       method = "##{method}" unless method =~ /^::/
#      :       command = "ri --format=rdoc #{clazz}#{method}"
#      :       Console[command].gsub(/\C-[.+?m/, '').gsub(/^/, '| ').gsub(/^\| +$/, '|')
#      :     end
#      :   end
#      : end
