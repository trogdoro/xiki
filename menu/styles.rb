module Xiki::Menu
  class Styles

    MENU = '
      - .font size/
      - .list faces/
      - api/
        > Summary
        | How to use the Styles class.  You can change the color and font of text.
        | You define styles, then make them apply to the text that matches
        | regular expression.

        > Define
        | Styles.define :red, :bg => "d77"

        > Apply
        | Styles.apply "apply", :red

        > Define more complex font
        | Styles.define :blueish,
        |   :fg => "99e",
        |   :face => "verdana",
        |   :size => 90,
        |   :bold => true

        > See
        | For styling specific text (not just a pattern):
        <<< @overlay/
      - see/
        <<< @css/list/
        <<< @themes/
      '

    def self.font_size size=nil
      ::Styles.font_size size
    end

    def self.list_faces
      ::Styles.list_faces
    end

end; end
