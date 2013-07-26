module Xiki::Menu
  class Themes
    MENU = "
      - .list/
      - dir/
        @$x/etc/themes/
      - docs/
        > What themes control
        | The window dividers (mode line) or all fonts and bg colors.
        | So, it can make sense to use multiple themes at once.
      "

    def self.list name=nil
      # /list/, so list themes...

      if ! name
        path = "#{Xiki.dir}etc/themes/"
        themes = Dir.new(path).entries.select{|o| o =~ /^\w/}
        return themes.map{|o| "- #{o[/\w+/].gsub('_', ' ')}/\n"}.join()
      end

      # /list/name, so use or navigate to theme...

      if Keys.open?   # If as+open, just navigate to it
        path = "#{Xiki.dir}etc/themes/#{TextUtil.title_case name, :underscores=>1}.notes"
        View.open path
        return
      end

      ::Xiki::Themes.use name
      Styles.reload_styles
      "@flash/- updated!"
    end

end; end
