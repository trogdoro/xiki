module Xiki
  class OptionItems

    # Makes copy of items, with tilde prepended (used for routing)
    def self.prepend_asterisk option_item

      # Put tilda before 1st item
      if option_item[0]
        option_item = option_item.dup   # Don't affect original option
        option_item[0] = "* #{option_item[0]}"
      end

      option_item

    end
  end
end
