# gem 'RedCloth'
# require 'redcloth'

module Xiki
  class Textile

    def self.render txt

      html = RedCloth.new(txt).to_html
      html << Html.default_css

      html
    end

    def self.menu *args

      # If nothing passed, show example textile
      return "
        > Render the textile wiki format in the browser
        | h1. Heading
        |
        | h2. Small Heading
        |
        | * Bullet
        | ** Another
        |
        | A normal sentence.
        |
        | bc.. Code can be displayed
        | like this.
        " if args.blank?

      html = self.render(ENV['txt'])

      Browser.html html

    end
  end
end
