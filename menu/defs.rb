class Defs
  #   MENU_HIDDEN = %Q`
  #     .pre patterns/
  #     `

  MENU = %Q`
    @menu_path/
    - .menus/
    - .patterns/
      - global/
      - pre/
    - .keys/
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
    - todo/
      - mockup of next version/
        @defs/
          @menu_path/
          + menus/
          + patterns/
            - global/
              | ^([$%&]) (.+)
              | ^select 
              | ^/[\w. _-]($|/)
              | ^zzz
            - word/
            - extension/
            - pre/
              - global/
              - word/
              - target view/
          + keys/
          + docs/
          + clear/
    `

  def self.menus key=nil, source=nil

    # /, so list menus...

    if ! key
      keys = Menu.defs.keys.sort.map{|o| "+ #{o}/"}
      return keys.join "\n"
    end

    # /key/, so show def...

    implementation = Menu.defs[key]

    self.defs implementation, source
  end

  def self.patterns category=nil, key=nil, source=nil

    # /, so list menus...

    hash = category == "pre" ? PrePattern.defs : Pattern.defs[:global]

    if ! key
      keys = hash.keys.map{|o| "| #{o.source rescue o}"}
      return keys.join "\n"
    end

    # /pattern/, so show def...

    regex = key[/^\| ?(.+)/, 1]

    found = hash.find{|k, v| k.source == regex}
    implementation = hash[found[0]]

    self.defs implementation, source

  end

  def self.defs implementation, source=nil

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
    "@flash/- cleared!"
  end

end
