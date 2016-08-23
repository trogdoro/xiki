module Xiki
  class Specials
    MENU = "
      - see/
        - keys/
          > Find next special char in this view
          - search+just+special
        <@ encoding/
        @ascii/
      "

    def self.menu_after output, *args

      return if args != []

      # /, so add output...

      options = yield
      ancestors = options[:ancestors]
Ol "ancestors", ancestors
      return "no ancestor???" if true#__________

      dir = Tree.dir :file=>1
Ol "dir", dir

      txt = dir
#      txt = `od -ct uC #{dir}`.gsub(/^/, '| ')

      "#{txt}#{output}"
    rescue RuntimeError=>e
      raise e if e.message !~ /This menu must be nested/   # Only handle if it's the complaining about not nesting under a dir

      "#{e.message}\n#{output}"

    end
  end
end
