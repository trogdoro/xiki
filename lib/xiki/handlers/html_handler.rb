module Xiki
  class HtmlHandler
    def self.handle options

      source = options[:handlers]['html']
      return if ! source || options[:output] || options[:halt]

      Browser.url "http://localhost:8163/#{options[:name]}/"

      View.flash "- showing in browser!"
      options[:output] = ""
    end
  end
end
