module Xiki
  class MenuHandler
    def self.handle options
      source_file = options[:ex]['menu']
      return if ! source_file || options[:output] || options[:halt]
      file = "#{options[:enclosing_source_dir]}#{source_file}"

      txt = File.read file, *Xiki::Files.encoding_binary
      txt.gsub! "\r\n", "\n"   # In case dos linebreaks

      path = Path.join(options[:args]||[])

      txt = Tree.children txt, path, options
      self.eval_when_exclamations txt, options

      options[:output] = txt
    end

    # If started with !, eval code...
    def self.eval_when_exclamations txt, options={}

      return if txt !~ /^! / || Keys.prefix == "source"

      source_file = options[:sources][-1][options[:source_index]]
      source_file = "#{options[:enclosing_source_dir]}#{source_file}"

      line_number = options[:children_line]
      line_number += 4 if source_file =~ /\.rb$/

      # TODO: to tighten up, only do this if all lines start with "!"

      code = txt.gsub /^! ?/, ''

      exclamations_args = options[:exclamations_args] || []

      # Run code based on whether a comment was found

      if code =~ /\A.* \/\/ \.js/
        returned = JavascriptHandler.eval code, options.merge(:file=>source_file)
      elsif code =~ /\A.* # \.coffee/   # If Coffeescript
        returned = CoffeeHandler.eval code, options.merge(:file=>source_file)
      elsif code =~ /\A.* # \.py/   # If Coffeescript  # If Python
        returned = PythonHandler.eval code, options.merge(:file=>source_file)
      else   # If Ruby
        code = "args = #{exclamations_args.inspect}\n#{code}"
        returned, out, exception = Code.eval code, source_file, line_number
      end

      returned ||= out || ""   # Use output if nothing returned
      returned = returned.to_s if returned

      txt.replace(
        if exception
          CodeTree.draw_exception exception, code
        else
          returned   # Otherwise, just return return value or stdout!"
        end
        )
    end
  end
end
