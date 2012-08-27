class Browse
  def self.menu file=nil, heading=nil, *content

    return "| Pass a bookmark and then use this menu to browse headings\n| in the file, such as:\n|\n@ browse/$t/" if file == "docs"

    return View.prompt "Type a bookmark of a .notes file." if file.nil?   # If no file, say it's required

    # Add dollar sign if not there
    file = "$#{file}" unless file =~ /^\$/

    Notes.drill "#{file}", heading, *content
  end
end
