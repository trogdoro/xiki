module Xiki
  class Tabs
    def self.menu *args

      # If no args, show option that toggles...

      return $el.elvar.tabbar_mode ? "- hide/" : "- show/" if args.empty?

      # Hidden "toggle" option

      if args == ['toggle']
        $el.customize_set_variable :tabbar_mode, ! $el.elvar.tabbar_mode
        return "<! toggled"
      end

      # Option was passed in, so do it...

      turn_on = args == ['show']

      $el.customize_set_variable :tabbar_mode, turn_on

      new_menu_item = turn_on ? "hide/" : "show/"
      Tree.replace_item new_menu_item
      Effects.glow :fade_in=>1

      nil
    end
  end
end
