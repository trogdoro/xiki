class SampleMenus
  MENU_HIDDEN = "
    .ajax/
    "

  def self.directions *args
    %q`
    | 1. Change the above text.
    | 2. Double-click on the text to create the file,
    |    thus creating the menu.
    | 3. Double-click on "<name>/" above to collapse,
    |    then double-click again to expand your new menu.
    |    Now you can type "<name>" on any blank line and
    |    double-click it!
    `
  end

  # Just filter out examples - for ajax calls from web create.
  def self.ajax *items
    txt = Xiki["sample menus/#{items * '/'}"]
    extension = txt[/.\w+$/]
    txt = txt.grep(/^ *\|/).join("").gsub(/^ *\| ?/, "")
    {:txt=>txt, :extension=>extension}.to_json
  end

end
