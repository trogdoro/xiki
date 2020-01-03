require "cgi"

module Xiki::Menu
  class Google
    def self.menu *args

      if args.empty?   # If no path, pull from history
        return "<? Type a search string"
      end

      # If multiple args, they were slash-delimited

      txt = args.length > 1 ?
        args.join("/") : args[0]

      if txt =~ /\n/

        # Multi-line, so convert to single line...

        txt.gsub!("\n", " ")

      elsif txt =~ /^:/
        txt.sub! /^: /, ''

      elsif Line !~ /google\//

        # Words not quoted (and not single "google/foo" line) so grab siblings...

        txt = Tree.siblings.join(" ")

      end

      Xiki::Google.search txt, :via_os=>(Keys.prefix_u ? nil : 1)

      nil
    end

  end
end
