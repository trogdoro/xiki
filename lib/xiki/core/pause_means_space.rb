require 'xiki/core/keys'

module Xiki
  class PauseMeansSpace

    def self.go
      Cursor.remember :before_q
      Cursor.box

      while(inserted = insert_until_pause)
        next if inserted == 127
        if inserted == "."
          $el.delete_backward_char 2;  insert ".  "
          next

        end
        insert " "
      end
      $el.delete_backward_char 1
      $el.message "Typing finished"

      Cursor.restore :before_q
      # TODO print ---- if enter
    end

    def self.insert_until_pause
      # Get first char and insert
      c = $el.read_char("insert text (pause means space): ")

      # Exit if they pressed enter
      return false if c == 13

      # Delete if they pressed delete
      if c == 127
        $el.backward_kill_word 1
        return 127
      end

      inserted = "#{c.chr}"
      insert c.chr

      o = $el.make_overlay $el.point, $el.point - 1

      # TODO use different face - purple?

      $el.overlay_put o, :face, :control_lock_found
      # While no pause, insert more chars
      while(c = read_char("insert text (pause means space): ", nil, 0.24))
        $el.delete_overlay o

        # Exit if they pressed enter
        return false if c == 13
        inserted += c.chr
        insert c.chr

        o = $el.make_overlay $el.point, $el.point - inserted.size
        $el.overlay_put o, :face, :control_lock_found
      end
      $el.delete_overlay o

      # User didn't press enter
      return inserted

    end
    def self.keys
      Keys.set("C-S-q", "PauseMeansSpace.go")
    end

  end
end
