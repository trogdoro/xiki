class Filter
  def self.menu filter=nil, *txt

    # Get parent file path!
      # If none, assume current file

    # For now, just assume it's the current file

    if ! filter   # If nothing passed, tell them to add something
      Line.add_slash
      return View.message "Add a something to filter by!" #, :beep=>1
    end

    if txt.blank?   # If no line passed, show lines
      return View.txt.grep(/#{filter}/).join("").gsub(/^/, '| ')
    end

    txt = txt.join "/"
    txt.sub! /^\| /, ''

    Move.to_end
    Search.forward "^#{txt}$"

    Move.to_line_text_beginning
    nil

  end
end
