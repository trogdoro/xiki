module Xiki
  class HtmlHandler
    def self.handle options
      source = options[:ex]['html']
      return if ! source || options[:output] || options[:halt]
      options[:output] = File.read "#{options[:last_source_dir]}#{source}"
    end
  end
end
