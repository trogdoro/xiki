class Facts
  def self.menu
    "
    > Example
    | France : Paris
    | England : London
    | Japan : Tokyo
    | Germany : Berlin

    - docs/
      | Add some facts like the example, and launch one of the
      | example lines to start an interactive memorize process.
      | In addition to helping you memorize, this is a low-stress
      | way to digest and review facts.
      | Double-click on one of to lines to begin.
    "
  end

  def self.menu_hidden
    "
    - .show answer/
    - .I was wrong/
    - .I was right/
    "
  end

  def self.i_was_right

    # Delete 2 controls...

    View.delete Line.left(0), Line.left(2)

    # If finished, just say so...

    if Line.blank?
      Line << "  - you're finished!\n"
      return
    end

    upcoming = Line.value
    upcoming.sub! "...", " "
    upcoming.sub! /(.+) : (.+)/, "\\1 : ?"

    # Delete top one if it also exists below (don't leave there until all are correct)...

    correct = Line.value 0

    left = View.cursor

    self.skip_past_hidden

    pending = View.txt left, View.cursor
    pending = pending.gsub("...", " ").split("\n")
    repeated = pending.member? correct

    View.cursor = left

    if repeated
      View.delete Line.left(0), Line.left(1)
    end

    View << "#{upcoming}\n  - show answer/\n"
    Line.previous
    nil
  end

  def self.i_was_wrong

    # Delete question and 2 controls...

    Line.previous
    wrong_line = Line.value
    View.delete Line.left(1), Line.left(4)
    left = View.cursor

    # If none left, just insert current one
    if Line.blank?
      wrong_line.sub!(/(.+) : (.+)/, "\\1 : ?")
      View << "#{wrong_line}\n"
      View.<< "  - show answer/\n"#, :dont_move=>1
      wrong_line.sub! "  | ", "  |..."
      View.<< "#{wrong_line}\n"
      Line.previous 2
      return
    end
    wrong_line.sub! "  | ", "  |..."

    # Change next one to question mark...

    txt = Line.value
    View << txt.sub(/(.+) : (.+)/, "\\1 : ?")+"\n"

    # Insert wrong answer 2 down...

    2.times { Line.next if Line =~ /\.\.\./ }
    View << "#{wrong_line}\n"

    # Insert wrong answer at end...

    self.skip_past_hidden
    Line.to_left
    View << "#{wrong_line}\n"

    View.cursor = left

    Line.sub! "...", " "

    Line.next
    View.<< "  - show answer/\n", :dont_move=>1

    nil
  end

  def self.skip_past_hidden
    Line.next while Line =~ /^ *\|\.\.\./
  end

  def self.show_answer
    Line.previous
    View.delete Line.left(1), Line.left(3)   # Delete question and control

    Line.sub! "...", " "   # Expose current line

    Line.next
    View << "  - I was wrong/\n  - I was right/\n"
    Line.previous 2

    nil
  end

  def self.menu_after output, *path

    # If quoted line, they're starting out...

    starting_out = path[0] && path[0] =~ /^\|/
    return if ! starting_out

    Search.backward "^[^:]*$"   # Find line above without a colon

    left = Line.left 2
    Line.next
    Search.forward "^[^:\n]*$"   # Find line below without a colon
    right = View.cursor

    txt = View.delete left, right
    txt = txt.split "\n"
    txt = txt.sort_by{ rand }
    first = txt[0]
    first = first.sub /(.+) : (.+)/, "\\1 : ?"

    pending = txt.join("\n")+"\n"
    pending.gsub!(/  \| /, "  |...")
    View.<< "#{first}\n  - show answer/\n#{pending}", :dont_move=>1

    Line.next

    nil
  end

end
