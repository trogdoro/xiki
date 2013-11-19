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
    txt = File.read __FILE__.sub(/\.rb$/, '.menu')
    txt = Xiki::Tree.children txt, items

    extension = txt[/.\w+$/]
    txt = txt.grep(/^ *\|/).join("").gsub(/^ *\| ?/, "")

    {:txt=>txt, :create_extension=>extension}.to_json
  end

  def self.by_extension extension

    return nil if extension == "."

    txt = File.read __FILE__.sub(/\.rb$/, '.menu')
    txt = txt[/#{extension}\n(^ +\|.+\n)+/]
    return nil if ! txt
    # Remove 1st line and pipes
    txt.sub! /.+\n/, ''
    txt.gsub! /^ +\| /, ''

    txt
  end

end
