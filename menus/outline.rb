class Outline
  def self.menu *target

    if target.blank?   # If just filter, show results
      return View.txt.grep(/^> /).select{|o| o !~ /^> ?$/}.join("").gsub(/^/, '| ')
    end

    # Navigated to target text

    target = Line.value
    target.sub! /^ *\| /, ''

    View.to_highest
    Search.forward "^#{target}$"

    Move.to_line_text_beginning
    nil

  end
end
