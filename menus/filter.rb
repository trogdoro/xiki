class Filter
  def self.menu filter=nil, *target

    # Get parent file path!
      # If none, assume current file

    # For now, just assume it's the current file

    # If nothing passed, tell them to add something
    return View.prompt "Type something to filter by" if ! filter

    return Tree.children(self.docs, target) if filter == "docs"

    if target.blank?   # If just filter, show results
      return View.txt.grep(/#{filter}/i).join("").gsub(/^/, '| ')
    end

    # Navigated to a target

    target = Line.value
    target.sub! /^ *\| /, ''

    View.to_highest
    Search.forward "^#{target}$"

    Line.to_beginning
    nil
  end

  def self.docs
    "
    > Examples
    | Filter the whole file for the string 'tt'
    @filter/tt/

    | Filter for NOT the string 'tt'
    @filter/!/tt/

    | Filter children for the string 'tt'
    @filter/tt/
      | Hat
      | Hatty
    "
  end

end
