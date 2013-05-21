class Mark
  MENU = "
    - .next/
    - .previous/
    - .outline/
    - .show/
    - light/
    - red/
    - orange/
    - yellow/
    - green/
    - blue/
    - purple/
    - white/
    - .delete/
    - .clear/
    "

  def self.menu_after output, *args
    return if ! Color.colors_by_name[args[0]]   # Only continue if arg is an existing color

    Xiki::Color.mark args[0]
    ""
  end

  def self.next
    Xiki::Color.next
    ""
  end
  def self.previous
    Xiki::Color.previous
    ""
  end

  def self.outline
    View.kill if View.name == "@mark/"

    txt = self.get_marked_lines
    if txt.blank?
      txt = "    - no marked lines in this view!"
    else
      txt.gsub! /^/, "    | "
    end

    file = View.file

    path = file ?
      "- #{File.expand_path(file).sub(/(.+)\//, "\\1/\n  - ")}\n" :
      "- buffer #{View.name}/\n"

    txt = "#{path}#{txt}"

    Launcher.open txt, :no_launch=>1

    ""
  end


  def self.get_marked_lines label=nil
    overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
    txt = ""
    overlays.to_a.reverse.each do |o|   # Loop through and copy all
      if label
        next if $el.overlay_get(o, :face).to_s != label
      end
      line = View.txt($el.overlay_start(o), $el.overlay_end(o))
      line << "\n" unless line =~ /\n$/
      txt << line
    end
    txt
  end

  def self.show
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
        txt << "    | #{line}"
      else # Else, show file and line
        txt << "@#{file.sub /(.+\/)/, "\\1\n  - "}\n    | #{line}"
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

    Tree.to_parent :u
    Move.to_quote

    ""
  end

  def self.delete
    View.kill if View.name == "@mark/"

    overlays = $el.overlays_at($el.next_overlay_change($el.point_at_bol - 1))
    return View.beep "- No highlights after cursor!" if ! overlays
    options = yield
    return $el.delete_overlay(overlays[0])
  end

  def self.clear
    View.kill if View.name == "@mark/"

    if Keys.prefix_u   # Don't delete map mark
      return $el.remove_overlays
    end

    overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
    overlays.to_a.reverse.each do |o|   # Loop through and copy all
      if $el.overlay_get(o, :face).to_s != "color-rb-light"
        $el.delete_overlay(o)
      end
    end

    ""
  end
end

