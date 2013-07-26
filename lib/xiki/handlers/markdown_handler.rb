# gem 'redcarpet'
# require 'redcarpet'

module Xiki
  class MarkdownHandler
    def self.handle options
      source = options[:ex]['markdown']
      return if ! source || options[:output] || options[:halt]

      txt = options[:output] = File.read "#{options[:enclosing_source_dir]}#{source}"
      html = self.render txt

      Browser.html html

      options[:output] = "@flash/- showing in browser!"
    end

    def self.render txt
      markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML, :autolink=>true, :space_after_headers=>true)
      html = markdown.render txt
      html << Html.default_css

      html
    end

  end
end
