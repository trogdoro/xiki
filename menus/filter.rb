class Filter
  def self.menu filter=nil, *target

    # Get parent file path!
      # If none, assume current file

    # For now, just assume it's the current file

    if ! filter   # If nothing passed, tell them to add something
      return View.prompt "Type something to filter by"
    end

    if target.blank?   # If just filter, show results
      return View.txt.grep(/#{filter}/i).join("").gsub(/^/, '| ')
    end

    # Navigated to a target

    target = Line.value
    target.sub! /^ *\| /, ''

    View.to_highest
    Search.forward "^#{target}$"

    Move.to_line_text_beginning
    nil

  end
end
