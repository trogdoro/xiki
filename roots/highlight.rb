class Highlight
  MENU = "
    - .all/
    - .show/
  "

  def self.all
    hash = Xiki::Color.all_marks_hash

    if hash.empty?   # If no marks found, just say so
      return "- no marks found!"
    end

    keys = hash.keys.sort.reverse

    txt = ""
    last_file = nil
    keys.each do |key|
      file, line = hash[key]
      if last_file == file   # If same file as last, just add line
        txt << "    : #{line}"
      else # Else, show file and line
        txt << "= #{file.sub /(.+\/)/, "\\1\n  - "}\n    : #{line}"
      end

      last_file = file
    end

    Tree.<< txt, :no_search=>1

    # Apply colors...

    keys.each do |key|
      Move.to_quote
      over = $el.make_overlay(Line.left+Line.indent.length, Line.right+1)
      $el.overlay_put over, :face, hash[key]
    end

    Tree.to_parent :prefix=>:u
    Move.to_quote

    ""
  end

end

