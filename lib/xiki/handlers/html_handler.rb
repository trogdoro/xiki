module Xiki
  class HtmlHandler
    def self.handle options

      source = options[:ex]['html']
      return if ! source || options[:output] || options[:halt]

      txt = File.read "#{options[:enclosing_source_dir]}#{source}"

      # Quote if it's calling the menu from the editor
      txt = Tree.quote(txt) if options[:client] =~ /^editor\b/

      options[:output] = txt
    end
  end
end
