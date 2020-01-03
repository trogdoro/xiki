
module Xiki
  class ErbHandler
    def self.handle options
      source = options[:handlers]['erb']
      return if ! source || options[:output] || options[:halt]

      require 'erubis'

      txt = File.read "#{options[:enclosing_source_dir]}#{source}"

      # template = File.read("/tmp/sample_file.erb")
      template = Erubis::Eruby.new(txt)
      txt = template.result(options)

      options[:output] = txt
    end
  end
end
