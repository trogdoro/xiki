module Xiki::Menu
  class Themes

    def self.menu name=nil
      # /list/, so list themes...

      if ! name
        path = "#{Xiki.dir}misc/themes/"
        themes = Dir.new(path).entries.select{|o| o =~ /^\w/}
        return themes.map{|o| "- #{o[/\w+/].gsub('_', ' ')}\n"}.join()
      end

      # /list/name, so use or navigate to theme...

      options = yield
      return "~ activate\n~ source" if options[:task] == []

      if options[:task] == ["source"] || Keys.open?   # If as+open, just navigate to it
        path = "#{Xiki.dir}misc/themes/#{TextUtil.title_case name, :underscores=>1}.notes"
        View.open path
        return
      end

      ::Xiki::Themes.use name
      Styles.reload_styles

      "<! activated!"
    end

  end
end
