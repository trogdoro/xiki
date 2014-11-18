class Substitute
  MENU_HIDDEN = %`
    + .docs/
    `

  # If store values
  def self.menu *args

    # /, so show current clipboard values...
    if args.empty?
      return "#{Clipboard[1].gsub(/\A|\n/, "\\0|-")}\n#{Clipboard[2].gsub(/\A|\n/, "\\0|+")}"
    end

    # /|-..., so save into clipboards...

    txt = Tree.quoted

    a = txt.scan(/^-(.*)/).join("\n")
    b = txt.scan(/^\+(.*)/).join("\n")

    Clipboard[1], Clipboard[2] = a, b

    "<! Now do do+1 to search and substitute!"

  rescue
    "
    > Error
    | Do as+1 and as+2 first.
    "
  end

  def self.docs
    %`
    | Shortcuts for setting the "1" and "2" clipboards, so you can
    | subsequently do do+1 to search and substitute.
    |
    | Example:
    =substitute/
      |-foo
      |+bar
    `
  end

end
