module Xiki
  class Hide

    @@visible = {}

    def self.menu
      "
      | TODO: add docs about hiding.
      "
    end


    #  self.reset
    def self.hide_unless search, options={}
      self.hide_unless_block(options) do |l, bol, eol|
        l =~ search
      end
    end

    def self.visible
      @@visible
    end

    def self.already_hidden
      @@already_hidden
    end

    def self.hide_unless_block options={}, &block

      #    elvar.line_move_ignore_invisible = true
      #     @@already_hidden = {}
      #     @@visible = []

      @@visible = []

      # Unless we can optimize by assuming hidden regions are ok
      unless options[:optimize]
        self.reset
        # Lisp funtion defined below
        l = $el.hide_find_overlays.to_a
        l.each { |i| @@already_hidden[i] = true }
      end

      t = $el.buffer_string
      lines = t.split "\n"
      bol = 1
      left = 1
      in_non_match = false

      if options[:include_first]   # Optionally skip top line
        l = lines.shift
        eol = bol + l.size + 1
        left = eol
        bol = eol
      end

      lines.each do |l|   # For each line
        eol = bol + l.size + 1
        #      if (l =~ search) && (! @@already_hidden[bol])
        if (yield(l, bol, eol)) && (! @@already_hidden[bol])
          @@visible << bol
          # If we're at the end of non-match area, hide
          if in_non_match
            @@areas_to_hide << [left, bol]
            in_non_match = false
          end
          left = eol
        else
          in_non_match = true
          @@already_hidden[bol] = true
        end
        bol = eol
      end

      @@areas_to_hide << [left, bol] if in_non_match

      # Construct and run lisp to hide it
      elisp = "(progn\n"
      @@areas_to_hide.each do |i|
        elisp += " (overlay-put (make-overlay #{i[0]} #{i[1]}) 'invisible 'hide)\n"
      end
      $el.el4r_lisp_eval elisp + ")"

    end

    def self.char_from_user
      c = $el.read_char_exclusive
      # Treat C-0 - C-9 the same as 0-9
      c -= 67108864 if(c >= 67108912 && c <= 67108921)
      "#{c.chr}"
    end

    def self.search search=nil, options={}
      if search
        if search.class == String
          search = /#{Regexp.escape(search)}/i
        end
        self.hide_unless search, options
      else
        search = ""
      end

      $el.message "Hide.search for: #{search}"
      ch = self.char_from_user

      first_search = true

      while true
        break unless ch =~ /[-a-zA-Z ._=@\/\\#,;*<>-]/
        search += ch
        $el.message "hide-search for: #{search}"

        # If a tab, clear search and keep going
        if ch == ","
          search = ""
          ch = self.char_from_user
          next
        end

        self.hide_unless /#{Regexp.escape(search)}/i, options
        $el.recenter -3

        # Don't re-check visible regions from now on
        if first_search
          options.merge!(:optimize => true)
          first_search = false
        end

        # If only one left, go to it
        if @@visible.size == 1 && options[:expand_when_one]
          ch = "1"
          break
        end
        ch = self.char_from_user
      end

      ch = 1 if ch == "\t"   # If a tab, treat it like C-1

      # If num
      if 1 <= ch.to_i && ch.to_i <= 9
        if @@visible.size > 0
          if @@visible[ch.to_i - 1]
            $el.goto_char @@visible[ch.to_i - 1]
          else
            $el.goto_char @@visible[-1]
          end
        else
          $el.goto_line ch.to_i
        end
        #      next_line -1
        self.show
        $el.recenter 0
        # Run enter if option was passed
        if options[:press_enter]
          $el.command_execute "\C-m"
        end

      # Show all if they typed C-a
      elsif ch == "\001"
        self.show
      # Else, if control character, execute it and exit
      elsif "\C-a" <= ch && ch <= "\C-z"
        $el.command_execute ch unless ch == "\C-m"
      end
    end

    def self.keys
    end

    # Shows things hidden by .hide
    def self.show

      # Move down to visible line (this doesn't seem to happen on its own)
      while( @@already_hidden[$el.point] )
        $el.forward_line
      end

      #    @@already_hidden = {}
      # Delete hidden overlays
      $el.el4r_lisp_eval %`
        (dolist (over (overlays-in (point-min) (point-max) ))
          (when
            (overlay-get over 'invisible)
            (delete-overlay over)
          )
        )
      `
      self.reset
      @@visible = []
    end

    def self.visible
      @@visible
    end

    def self.reset
      @@areas_to_hide = []
      # Make this be stored per page, not globally
      @@already_hidden = {}
    end

    # What is this doing here??
    self.reset
    $el.add_to_invisibility_spec :hide if $el

    def self.hide_by_indent indent=nil
      indent ||= Keys.prefix
      if indent == :u
        $el.set_selective_display nil
        return
      end

      # If no prefix, use indent of current line
      if indent.nil?
        indent = Line.matches(/^ */).size
        # If currently indented to that level,
        if $el.elvar.selective_display && indent == ($el.elvar.selective_display - 1)
          indent += 2
        end
      end

      $el.set_selective_display(indent + 1)
    end

    def self.init

      return if ! $el

      # Define lisp function to get list of displayed lines
      # In case something has been done to change them
      $el.el4r_lisp_eval %`
        (defun hide-find-overlays ()
          (save-excursion
            (beginning-of-buffer)
            (let (found (res ()))
              (while (not (eq (point) (point-max)))
                (setq found nil)
                ; Save if not hidden
                (dolist (o (overlays-at (point)))
                  (if (overlay-get o 'invisible)
                    (setq found t)
                  )
                )
                (if found
                  (push (point) res))
                (forward-line)
              )
              (reverse res)
            )
          )
        )
      `
    end

    # Reveals all hidden
    def self.reveal
      $el.widen
      self.show
      self.hide_by_indent :u   # If hidden by indent
    end

    # Hides using narrowing
    def self.hide left, right
      $el.narrow_to_region left, right
      nil
    end


  end
  Hide.init
end
