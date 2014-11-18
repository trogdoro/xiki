module Xiki::Menu
  class Styles

    MENU = '
      - .font size/
      - .list faces/
      - api/
        - summary/
          | You can change the color and font of text using the Styles class.
          | You define styles, then make them apply to the text that matches
          | regular expression.
        - define/
          =Styles.red :bg => "7d7"
        - apply/
          =Styles.apply "apply", :red
        - define more complex font/
          | Stylesblueish
          |   :fg => "99e",
          |   :face => "verdana",
          |   :size => 90,
          |   :bold => true
        - see/
          | For styling a specific block of text.  Location-based, not regular
          | expression based.
          <<< =overlay/
      - see/
        <<< =css/list/
        <<< =themes/
      '

    def self.font_size size=nil
      ::Styles.font_size size
    end

    def self.list_faces
      ::Styles.list_faces
    end

end; end
