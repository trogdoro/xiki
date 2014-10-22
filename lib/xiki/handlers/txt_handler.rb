module Xiki
  class TxtHandler
    def self.handle options

      source = options[:handlers]['txt']
      return if ! source || options[:output] || options[:halt]

      file = "#{options[:enclosing_source_dir]}#{source}"

      # /, so just show the contents...

      if ! options[:args]

        txt = File.read file

        if options[:client] =~ /^web\//
          txt = "<pre>#{txt}</pre>"
          return options[:output] = txt
        end

        return options[:output] = Tree.quote(txt, :char=>"|")
      end

      # An arg passed, so must want to save...

      # =commit/.txt > make it save upon launch

      File.open(file, "w") { |f| f << options[:args][0] }
      options[:output] = "<! saved"

    end
  end
end
