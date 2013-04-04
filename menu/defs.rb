class Defs
  def self.menu
    %Q`
    > About
    - docs/
      - todo: pull menus from...!
      @expander/docs/

      > Ways to define expanders
      - menu/
        - block/
          @Xiki.def(:foo) {"Some code"}
        - file backed/
          @Xiki.def("/tmp/foo.rb")
        - multiple file backed/
          @Xiki.def("/tmp/foo")   # Maybe /tmp/foo.rb and /tmp/foo.menu exist

        | Try it out:
        @foo
      - pattern/
        @Xiki.def(/^bar /) {"Some code"}

        | Try it out:
        @barbary
      - key shortcut/
        - block/
          @Xiki.def("do+hi") { View << "hi" }
        - open a menu/
          @Xiki.def "do+hi", "ip"
        - insert a menu/
          @Xiki.def "enter+hi", "ip"
        | Try it by typing the acronym of the shortcut you defined.
        | Example: Ctrl+d Ctrl+h for "do hi".
    - .clear/
    `
  end

  def self.menu_after output, *path

    # /, so add definitions to output

    if path == []
      menu_keys = Menu.defs.keys.sort.map{|o| "+ #{o}/"}
      pattern_keys = Pattern.defs.keys.map{|o| "| #{o.source}"}
      txt = (menu_keys + pattern_keys).join("\n")
      return "@menu_path/\n@keys/\n#{txt}\n#{output}"
    end

    return if output   # Do nothing if .menu already handled it

    self.defs *path
  end


  def self.defs key=nil, source=nil

    # /, so list all keys...

    implementation =
      if key =~ /^\|/
        regex = key[/^\| ?(.+)/, 1]
        found = Pattern.defs.find{|k, v| k.source == regex}
        Pattern.defs[found[0]]
      else
        Menu.defs[key]
      end

    # /key/, so show implementation...

    if ! source
      if implementation.is_a?(Proc)
        txt = implementation.source rescue "Proc: #{implementation.source_location.inspect}"
        return Tree.quote txt
      elsif implementation.is_a?(String)
        return "@#{implementation}/"
      else
        return "Don't recognize #{implementation.class}: #{implementation}"
      end
    end

    # /key/implementation, so jump to it...

    # For now, assume it's a block (though it'll be all kinds of things)

    file, line = implementation.source_location
    View.open file
    View.line = line
    nil

  end

  def self.clear
    Pattern.defs.clear
    Menu.defs.clear
    ".flash - cleared!"
  end

end
