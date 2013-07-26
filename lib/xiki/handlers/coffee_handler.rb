module Xiki
  class CoffeeHandler

    def self.handle options
      source = options[:ex]['coffee']
      return if ! source || options[:output] || options[:halt]

      source = "#{options[:enclosing_source_dir]}#{source}"
      txt = File.read source

      txt = self.eval txt, options.merge(:file=>source)
      options[:output] = txt
    end

    def self.eval txt, options={}
      txt = "
        func = ->
          args = #{JSON[options[:args] || []]}
        ".unindent+txt.gsub(/^/, "  ")+"
        console.log JSON.stringify func()
        ".unindent

      txt = Console.run "coffee --stdio", :sync=>true, :stdin=>txt
      lines = txt.split "\n"

      # Last line is the return value...

      returned = lines.pop

      out = lines.any? ? lines.join("\n") : nil
      Ol.a(out, :stack_line=>"#{options[:file]}:1:in `script'") if out

      if returned == "undefined"
        nil
      elsif returned !~ /^[{\[]/   # Because JSON has to be an array or hash
        JSON["[#{returned}]"][0]
      else
        JSON[returned]
      end

    rescue Exception=>e
      return CodeTree.draw_exception e
    end
  end
end
