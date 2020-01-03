module Xiki
  class DeckHandler
    def self.handle options
      source = options[:handlers]['deck']

      return if ! options[:handlers] || options[:output] || options[:halt]
      path = "#{options[:enclosing_source_dir]}#{source}"

      args = options[:args]

      options[:halt] = 1   # Just in case there's no output

      # /, so let drill show args...

      if ! args || options[:prefix]
        txt = Notes.drill path, *args||[]
        return options[:output] = "" if ! txt
        return options[:output] = txt.split("\n").uniq.join("\n")
      end

      # />heading, so let open and start...

      View.open path

      Deck.show_all

      heading = args.join '/'

      View.hide_others :all=>1

      Xiki["dimensions/presentation full/"]

      View.to_highest
      Search.forward "^#{$el.regexp_quote heading}$", :beginning=>1
      Deck.right_arrow :dont_move=>1

      options[:output] = ""

    end

  end
end
