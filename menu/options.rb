module Xiki
  class Options

    def self.menu *args

      # Grab parent subpath to parse and pre-expand

      path = yield[:ancestors]

      raise "> How to use\n| Nest this under a menu name\n" if ! path

      # .expanders calls .parse
      options = Expander.expanders path

      if options[:menufied]
        Menu.climb_sources options
      end

      # /, so show options...

      if args.empty?
        txt = self.ap options
        txt.gsub! /^  /, ""
        txt.sub! "[\n  [", "[["
        txt = Tree.quote txt
        txt << "<< source/" if options[:menufied]   # Sources menu helps for menus
        return txt
      end

      # /|:def..., so jump to it...

      file, line = Tree.source options

      return "@flash/- no source found!" if ! file

      View.open file
      View.line = line if line

      nil
    end

    def self.ap txt
      txt = txt.ai
      txt.sub! /\A{\n/, ''
      txt.sub! /\n}\z/, ''
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
