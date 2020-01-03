module Menu
  class Wikipedia

    def self.wp name=nil, options={}
      if name.nil?
        # TODO > how to deal with fact that history will have wp and wikipedia? - pass in list:
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

      name = ::Xiki::TextUtil.title_case name if name !~ /^[A-Z]/   # Titlize if not already

      options[:no_slash] = 1
      ::Xiki::Firefox.url "http://en.wikipedia.org/wiki/#{name}"
      "<* opened in browser!"
    end
  end

end
