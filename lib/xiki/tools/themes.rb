module Xiki
  #
  # Lists themes and run them. Make files here
  # to make more themes:
  #
  # @$x/etc/themes/
  #
  class Themes

    def self.use name
      path = "#{Xiki.dir}etc/themes/#{name.gsub " ", "_"}.notes"
      raise "- Theme doesn't exist!" if ! File.exists? path

      load path

      Styles.reload_styles

      nil
    end
  end
end
