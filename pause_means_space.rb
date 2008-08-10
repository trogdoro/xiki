require 'keys'

class PauseMeansSpace
  extend ElMixin
  def self.go
    Cursor.remember :before_q
    Cursor.green

    while(inserted = insert_until_pause)
      next if inserted == 127
      if inserted == "."
        delete_backward_char 2;  insert ".  "
        next

      end
      insert " "
    end
    delete_backward_char 1
    message "Typing finished"

    Cursor.restore :before_q
    # TODO print ---- if enter
  end

  def self.insert_until_pause
    # Get first char and insert
    c = read_char("insert text (pause means space): ")

    # Exit if they pressed enter
    return false if c == 13

    # Delete if they pressed delete
    if c == 127
      backward_kill_word 1
      return 127
    end

    inserted = "#{c.chr}"
    insert c.chr

    o = make_overlay point, point - 1

    # TODO use different face - purple?

    overlay_put o, :face, :control_lock_found
    # While no pause, insert more chars
    while(c = read_char("insert text (pause means space): ", nil, 0.24))
      delete_overlay o

      # Exit if they pressed enter
      return false if c == 13
      inserted += c.chr
      insert c.chr

      o = make_overlay point, point - inserted.size
      overlay_put o, :face, :control_lock_found
    end
    delete_overlay o

    # User didn't press enter
    return inserted

  end
  def self.keys
    Keys.set("C-S-q", "PauseMeansSpace.go")
  end

end
