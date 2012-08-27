#
# Lists themes and run them. Make files here
# to make more themes:
#
# @$x/etc/themes/
#
class Themes
  def self.menu
    "
    + .list/
    - dir/
      @$x/etc/themes/
    - docs/
      > What themes control
      | The window dividers (mode line) or all fonts and bg colors.
      | So, it can make sense to use multiple themes at once.
    "
  end

  def self.list name=nil

    # If nothing passed, list themes

    if ! name
      path = "#{Xiki.dir}etc/themes/"
      themes = Dir.new(path).entries.select{|o| o =~ /^\w/}
      return themes.map{|o| "- #{o[/\w+/].gsub('_', ' ')}/\n"}.join()
    end

    # Theme name passed

    if Keys.open?   # If as+open, just navigate to it
      path = "#{Xiki.dir}etc/themes/#{TextUtil.title_case name, :underscores=>1}.notes"
      View.open path
      return
    end

    # Just use theme

    self.use name

    ".flash - updated!"
  end

  def self.use name
    path = "#{Xiki.dir}etc/themes/#{TextUtil.title_case name, :underscores=>1}.notes"
    return "| Theme doesn't exist" if ! File.exists? path

    load path

    Styles.reload_styles

    nil
  end

end
