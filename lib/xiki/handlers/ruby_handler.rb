module Xiki
  class RubyHandler
    def self.handle options

      source = options[:handlers]['rb']
      return if ! source || options[:output] || options[:halt]
      clazz_name = TextUtil.camel_case source[/\w+/]

      file = "#{options[:enclosing_source_dir]}#{source}"
      code = File.read file

      options.merge! :dot_menu_file=>"#{options[:enclosing_source_dir]}#{options[:handlers]['menu']}"

      txt =
        if code =~ /^ *(class|module) .*\b#{clazz_name}\b/   # Maybe check for actual class name
          options.merge! :clazz_name=>clazz_name
          self.handle_class file, code, options
        else
          self.handle_script file, code, options
        end
      options[:output] = txt
    end

    def self.handle_class file, code, options
      Invoker.invoke file, options[:args], options.merge!(:code=>code)
    end

    def self.handle_script file, code, options

      # Pass in args and options?
      args = Code.args options #, :double_assign=>1
      code = "#{args}\n#{code}"

      txt, out, exception = Code.eval code, file, 0, {:pretty_exception=>1}, options

      # Print any output
      Ol.a(out, :stack_line=>"#{file}:1:in `script'") if out

      return exception if exception

      txt

    end
  end
end

