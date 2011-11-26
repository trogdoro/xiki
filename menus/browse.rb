class Browse
  def self.menu file=nil, heading=nil

    return "| Pass a bookmark and then use this menu to browse headings\n| in the file, such as:\n|\n@ browse/$t/" if file == "docs"

    return View.prompt "Type a bookmark to browse." if file.nil?   # If no file, say it's required

    Notes.drill file, heading
  end
end
