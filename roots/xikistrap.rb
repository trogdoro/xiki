class Xikistrap < Xiki::Menu::Bootstrap

  # Use our own version of the menu with "@xikistrap/" in it instead
  # of the superclass', which has "@bootstrap/" in it.
  MENU = self.menu_constant

  # Over-ride .render to the xiki logo before delegating to the
  # super's .render.
  def self.render txt
    navbar = %`
      - navbar/
        | <img src="http://xiki.org/images/bootstrap_icon.png" id="logo">
        | <style>.navbar .container { padding: 1px 30px 0px; }</style>
      - style/
        | body {
        |   background-color: #eee;
        | }
      `.unindent

    txt = "#{navbar}\n#{txt}"

    super txt
  end
end
