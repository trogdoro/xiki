module Xiki
  class JavascriptHandler
    def self.handle options
      source = options[:ex]['js']
      return if ! source || options[:output] || options[:halt]

      source = "#{options[:last_source_dir]}#{source}"
      txt = Console.run "js \"#{source}\"", :sync=>true
      options[:output] = txt
    end
  end
end
