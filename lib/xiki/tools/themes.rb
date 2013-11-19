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

      # This should be done manually, since many themes can be used in a row
      #       Styles.reload_styles

      nil
    end
  end
end
