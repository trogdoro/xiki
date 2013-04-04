class RubyHandler
  def self.handle options, ex

    return if ! ex['rb'] || options[:output]

    stem = ex['rb']
    clazz_name = TextUtil.camel_case stem[/\w+/]

    file = "#{options[:last_source_dir]}#{stem}"
    code = File.read file
    options.merge! :dot_menu_file=>"#{options[:last_source_dir]}#{ex['menu']}"
    txt =
      if code =~ /^ *class #{clazz_name}/   # Maybe check for actual class name
        options.merge! :clazz_name=>clazz_name
        self.handle_class file, code, options
      else
        self.handle_script file, code, options
      end
    options[:output] = txt
  end

  def self.handle_class file, code, options
    Invoker.invoke file, options[:args], options.merge(:code=>code)
  end

  def self.handle_script file, code, options
    # Pass in args and options?
    code = "args = #{(options[:args]||[]).inspect}\noptions = #{options.inspect}\n#{code}"

    returned, out, exception = Code.eval code, file, 3

    txt =
      if exception
        CodeTree.draw_exception exception, code
      else
        returned || out   # Otherwise, just return return value or stdout!"
      end

    txt
  end
end
