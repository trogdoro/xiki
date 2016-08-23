module Xiki
  class Handlers

    def self.menu *args

      # Grab parent subpath to parse and pre-expand

      options = yield
      ancestors = options[:ancestors]

      raise ": Nest this under another menu.\n<< all handlers/" if ! ancestors

      menu_steps = ancestors[-1] == "menu steps/"
      ancestors.pop if menu_steps

      path = ancestors.any? ? ancestors : options[:path]


      options_result = Expander.expanders path

      if options_result[:menufied]
        Command.climb_sources options_result
      end


      options_result_orig = options_result.dup

      Command.determine_handlers options_result

      options_result_orig.keys.each{|k| options_result.delete k}

      txt = self.ap options_result
      txt.gsub! /^  /, ""
      txt.sub! "[\n  [", "[["
      txt = Tree.quote txt, :char=>"|"

      txt << "<< all handlers/\n"

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
