class Outline
  def self.menu *target

    if target.blank?   # If just filter, show results
      return View.txt.grep(/^> /).select{|o| o !~ /^> ?$/}.join("").gsub(/^/, '| ')
    end
    # Navigated to target text

    # Grab line manually, because input will have all lines grouped together
      # Should there be an option to have just 1 line passed in?
        # How would it work?
          # Something in the routing?
    target = Line.value
    target.sub! /^ *\| /, ''

    View.to_highest
    result = Search.forward "^#{target}$"

    View.recenter_top
    Line.to_beginning
    nil

  end
end
