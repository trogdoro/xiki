#
# Lists themes and run them. Make files here
# to make more themes:
#
# @$x/etc/themes/
#
class Themes

  def self.use name
    path = "#{Xiki.dir}etc/themes/#{TextUtil.title_case name, :underscores=>1}.notes"
    return "| Theme doesn't exist" if ! File.exists? path

    load path

    Styles.reload_styles

    nil
  end
end
