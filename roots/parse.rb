module Xiki
  class Parse

    def self.menu *args

      # Grab parent subpath to parse and pre-expand

      options = yield
      ancestors = options[:ancestors]
      menu_steps = ancestors[-1] == "menu steps/"
      ancestors.pop if menu_steps

      path = ancestors.any? ? ancestors : options[:path]

      parse_result = Expander.parse path

      txt = self.ap parse_result
      txt.gsub! /^  /, ""
      txt.sub! "[\n  [", "[["
      txt = Tree.quote txt, :char=>"|"

      txt
    end

    def self.ap txt
      txt = txt.ai
      txt.sub! /\A{\n/, ''
      txt.sub! /\n}\z/, ''
      txt
    end

  end
end
