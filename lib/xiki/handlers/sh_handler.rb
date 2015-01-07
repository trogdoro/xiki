module Xiki
  class ShHandler


    def self.handle options
      source = options[:handlers]['sh']
      return if ! source || options[:output] || options[:halt]

      source = "#{options[:enclosing_source_dir]}#{source}"
      txt = File.read source

      txt = self.eval txt, options #.merge(:file=>source)
      options[:output] = txt
    end

    def self.eval txt, options={}

      args = options[:args]
      args = args ? args.map{|o| o.inspect}.join(" ") : ""

      txt = "ARGS=(#{args})\n"+txt.gsub(/^/, "  ")

      txt = Shell.run "bash", :sync=>true, :stdin=>txt

      txt

    rescue Exception=>e
      return CodeTree.draw_exception e
    end
  end
end
