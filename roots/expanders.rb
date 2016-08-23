module Xiki
  class Expanders

    def self.menu *args

      # Grab parent subpath to parse and pre-expand

      options = yield
      ancestors = options[:ancestors]
      menu_steps = ancestors[-1] == "menu steps/"
      ancestors.pop if menu_steps

      path = ancestors.any? ? ancestors : options[:path]

      expanders_result = Expander.expanders path

      txt = self.ap expanders_result
      txt.gsub! /^  /, ""
      txt.sub! "[\n  [", "[["
      txt = Tree.quote txt, :char=>"|"
      txt << "<= menu steps/" if ! menu_steps   # if options_result[:menufied]   # Sources menu helps for menus

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
