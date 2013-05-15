class Replace
  MENU = %`
    - docs/
      | Shortcuts for setting the "1" and "2" clipboards, so you can
      | subsequently do do+1 to search and replace.
      |
      | Example:
      @replace/
        | foo
        | bar
    `

  # If store values
  def self.menu_after output, txt=nil

    # /..., so prepend current clipboard values...

    if ! txt
      return ": #{Clipboard[1]}\n: #{Clipboard[2]}\n#{output}"
    end

    return if output   # Only do something if .menu outputted nothing

    # | before..., so save into clipboards...

    a, b = txt.split "\n"

    Clipboard[1], Clipboard[2] = a, b

    ".flash - Now do do+1 to search and replace"
  end
end
