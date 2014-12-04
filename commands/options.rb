module Xiki
  class Options

    def self.menu *args

      # Grab parent subpath to parse and pre-expand...

      options = yield

      ancestors = options[:ancestors]
      menu_steps = ancestors.any? && ancestors[-1] == "menu steps/"
      ancestors.pop if menu_steps

      # Normally, this is more complicated than just using this menu


      path = ancestors.any? ? ancestors : options[:path]

      options_result = Expander.expanders path

      if options_result[:menufied]
        Menu.climb_sources options_result
      end

      options_propigate = options.dup
      options_result = options.merge! options_result   # Merge in original options, so editor and prefix vars etc vars are there (might cause problems?)

      # /, so show options_result...

      txt = TextUtil.ap options_result
      txt = Tree.quote txt, :char=>"|"

      txt << "<= menu steps/" if ! menu_steps   # if options_result[:menufied]   # Sources menu helps for menus

      txt

    end

    def self.yaml txt
      txt = txt.to_yaml
      txt.sub! /\A--- \n/, ''
      txt.gsub!(/ $/, '')
      txt
    end

  end
end
