class Themes
  def self.menu *args

    # If nothing passed, show options

    if args.empty?
      path = "#{Xiki.dir}etc/themes/"

      themes = Dir.new(path).entries.select{|o| o =~ /^\w/}

      return "
        | Click one of these to change how the window dividers
        | look (aka the emacs mode line).
        #{themes.map{|o| "- #{o[/\w+/].gsub('_', ' ')}/\n"}.join()}
        "
    end

    # Theme name passed, so use it

    self.use args[0]
  end

  def self.use name
    path = "#{Xiki.dir}etc/themes/#{TextUtil.title_case name, :underscores=>1}.notes"
    return "| Theme doesn't exist" if ! File.exists? path

    load path

    nil
  end

end
