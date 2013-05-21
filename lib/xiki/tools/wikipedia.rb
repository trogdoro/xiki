module Xiki
  class Wikipedia

    def self.menu name=nil
      "
      > See
      << wp/
      "
    end

    def self.wp name=nil
      if name.nil?
        # TODO: how to deal with fact that history will have wp and wikipedia? - pass in list:
        # Maybe get the actual root pass it (wikipedia or wp)
          # How to get it?
            # Do: pass list into Launcher.last
              # Launcher.last(["wp", "wikipedia"], :exclude_path=>1)
              # or, just make it a regex?
                # And add ^...$ inside .last

        return (
          Launcher.last("wp", :exclude_path=>1) +
          Launcher.last("wikipedia", :exclude_path=>1)
          )

      end

      Firefox.url "http://en.wikipedia.org/wiki/#{TextUtil.title_case name}"
      ".flash - opened in browser!"
    end
  end

  Menu.wp {|path|
    Wikipedia.wp Tree.rootless(path)
  }
end
