module Xiki
  class JavascriptHandler
    def self.handle options
      source = options[:handlers]['js']
      return if ! source || options[:output] || options[:halt]

      source = "#{options[:enclosing_source_dir]}#{source}"

      txt = File.read source

      txt = self.eval txt, options.merge(:file=>source)

      options[:output] = txt # || ""
    end

    # JavascriptHandler.eval txt, options
    def self.eval txt, options={}

      task = options[:task] || '"null"'

      txt = "
        var output = (function(){
        args = #{JSON[options[:args] || []]};
        task = #{task};
        var p = console.log;
        ".unindent+txt+"
        })()
        console.log(JSON.stringify(output))
        ".unindent

      File.open("/tmp/node_tmp.js", "w") { |f| f << txt }
      txt = Shell.command "node /tmp/node_tmp.js", :dir=>options[:dir] #, :raise_error=>1

      # Raised error, so just show it

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
        return returned
      end

      # No returned value, use stdout lines as output...

      out

    rescue Exception=>e
      return CodeTree.draw_exception e
    end

  end
end
