module Xiki
  class JpgHandler
    def self.handle options
      source = options[:ex]['jpg']
      return if ! source || options[:output] || options[:halt]

      path = "#{options[:last_source_dir]}#{source}"

      options[:output] = "@file/#{path}"
    end
  end
end
