module Xiki
  class CoffeeHandler

    def self.handle options
      source = options[:handlers]['coffee']
      return if ! source || options[:output] || options[:halt]

      source = "#{options[:enclosing_source_dir]}#{source}"
      txt = File.read source

      txt = self.eval txt, options.merge(:file=>source)
      options[:output] = txt
    end

    def self.eval txt, options={}

      task = options[:task] || '"null"'
      txt = "
        func = ->
          args = #{JSON[options[:args] || []]}
          task = #{task}
        ".unindent+txt.gsub(/^/, "  ")+"
        console.log JSON.stringify func()
        ".unindent

      txt = Shell.command "coffee --stdio", :stdin=>txt #, :raise_error=>1

      return txt if txt =~ /\A\n?> error\n/

      lines = txt.split "\n"

      # Last line is the return value...

      returned = lines.pop
      returned =
        if returned == "undefined"
          nil
        elsif returned !~ /^[{\[]/   # Because JSON has to be an array or hash
          JSON["[#{returned}]"][0]
        else
          JSON[returned]
        end

      out = lines.any? ? lines.join("\n") : nil

      # Returned value, so use it, and Ol stdout lines...

      if returned
        Ol.a(out, :stack_line=>"#{options[:file]}:1:in `script'") if out
        return returned
      end

      # No returned value, use stdout lines as output...

      out

    rescue Exception=>e
      return CodeTree.draw_exception e
    end
  end
end
