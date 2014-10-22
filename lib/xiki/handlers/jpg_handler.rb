module Xiki
  class JpgHandler
    def self.handle options
      source = options[:handlers]['jpg']
      return if ! source || options[:output] || options[:halt]

      path = "#{options[:enclosing_source_dir]}#{source}"

      options[:output] = "@file/#{path}"
    end
  end
end
