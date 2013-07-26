module Xiki
  class Tabs
    def self.menu *args

      # If no args, show option that toggles...

      return $el.elvar.tabbar_mode ? "- hide tabs/" : "- show tabs/" if args.empty?

      # Option was passed in

      turn_on = args == ['show tabs']

      $el.customize_set_variable :tabbar_mode, turn_on

      new_menu_item = turn_on ? "hide tabs/" : "show tabs/"
      Tree.replace_item new_menu_item
      Effects.glow :fade_in=>1

      nil
    end
  end
end
