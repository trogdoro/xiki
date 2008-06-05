class Fc
  extend ElMixin

  @@ask_in_reverse = false

  # Randomize rows and gray out all but top one
  def self.start
    # Get  table
    @@top = point
    @@orig_top = point
    @@orig_lines = grab_lines

    # Replace with shuffled versions
    delete_region @@top, end_of_lines
    shuffle
    insert @@shuffled_lines.join("\n")
    insert "\n"

    clear_overlays

    # Make all but top be grayed
    goto_char @@top

    elvar.local_map_before_fc = current_local_map
    use_local_map elvar.fc_map

    ask_line
  end

  def self.correct
    # Ignore correct if they haven't seen answer yet
    return unless @@showed_answer

    @@showed_answer = false

    # Move top down one line
    goto_char @@top
    next_line
    @@top = point

    return if check_for_finished

    clear_overlays
    ask_line

  end

  def self.check_for_finished
    # We're done if there's nothing on this line

    if buffer_substring(point_at_bol, point_at_eol) =~ /^ *$/
      speak "You're finished", "Kathy"
      cancel
      return true
    end
    false
  end

  def self.cancel
    clear_overlays
    goto_char @@orig_top
    # TODO figure out how to restore old local map that was here
    use_local_map elvar.local_map_before_fc

  end

  def self.show_answer
    # If we haven't shown answer, show for first time
    unless @@showed_answer
      delete_overlays_on_line
      speak_question_or_answer 2
      @@showed_answer = true
    # They missed it (they have shown the answer)
    else
      clear_overlays
      # Delete line
      missed = thing_at_point(:line)
      delete_region point_at_bol, point_at_eol + 1

      # TODO Put line down after 2
      last_line = end_of_lines
      left_to_move = 2
      # Move down until we've gone 2 or are after end
      while( left_to_move > 0 && point < last_line )
        next_line
        left_to_move -= 1
      end
      insert missed

      # Put line to end
      goto_char end_of_lines
      insert missed

      ask_line
    end
  end

  def self.ask_in_reverse= to_this
    @@ask_in_reverse = to_this
  end

  def self.ask_line
    @@showed_answer = false
    goto_char @@top
    next_line
    gray_until_end
    gray_answer
    speak_question_or_answer 1
  end

  def self.repeat_question
    speak_question_or_answer 1
  end

  def self.delete_overlays_on_line
    os = overlays_in(point_at_bol, point_at_eol)
    return unless os
    os.each do |o|
      delete_overlay o
    end
  end

  def self.speak_question_or_answer q_or_a
    goto_char @@top
    thing_at_point(:line) =~ /(.+) ?\| ?(.+)/
    which_one = q_or_a == 1
    which_one = ! which_one if @@ask_in_reverse
    which_one ? speak($1) : speak($2, "Victoria")
  end


  def self.gray_answer
    goto_char @@top
    re_search_forward " | "
    unless @@ask_in_reverse
      overlay_put make_overlay(point, point_at_eol), :face, :fc_invisible
    else
      overlay_put make_overlay(point_at_bol, match_beginning(0)), :face, :fc_invisible
    end
  end

  def self.end_of_lines
    old = point
    re_search_forward /^$/
    r = point
    goto_char old
    r
  end

  def self.clear_overlays

    os = overlays_in(point_min, point_max)
    return unless os
    os.each do |o|
      delete_overlay o
    end
  end

  def self.gray_until_end
    top = point

    re_search_forward /^$/
    overlay_put make_overlay(top, end_of_lines), :face, :fc_invisible
  end

  def self.grab_lines
    buffer_substring(@@top, end_of_lines).split /\n/
  end

  def self.shuffle
    a = @@orig_lines.dup
    (a.size-1).downto(1) { |i|
        j = rand(i+1)
        a[i], a[j] = a[j], a[i] if i != j
    }
    @@shuffled_lines = a
  end

  def self.speak words, voice="Vicki"
    words.gsub! '"', '\"'
    words.gsub! '_', '-'
    do_applescript "say \"#{words}\" using \"#{voice}\""
  end

  def self.define_keys

    elvar.fc_map = make_sparse_keymap
    # Template > Defining 1 and 2 keys
    define_key(:fc_map, "1") { Fc.show_answer }
    define_key(:fc_map, "2") { Fc.correct }
    define_key(:fc_map, "3") { Fc.repeat_question }
    define_key(:fc_map, "0") { Fc.cancel }

    define_key(:fc_map, kbd("C-1")) { Fc.show_answer }
    define_key(:fc_map, kbd("C-2")) { Fc.correct }
    define_key(:fc_map, kbd("C-3")) { Fc.repeat_question }
    define_key(:fc_map, kbd("C-0")) { Fc.cancel }

#     # Template > Defining a key
#     Keys.DZ do
#       # If 2 passed as prefix, do reverse
#       Fc.ask_in_reverse = elvar.current_prefix_arg == 1
#       Fc.start
#       # TODO figure out how to save local map to restore it later
#       # store this? current-local-map
#       use_local_map elvar.fc_map
#     end

  end

  # Define font
  def self.define_styles
    el4r_lisp_eval <<-'EOL'
      (progn
        (set-face-attribute (make-face 'fc-invisible) nil
          :foreground "#ffffff")
        )
    EOL
  end
  def self.init
    Fc.define_styles
    Fc.define_keys
  end
end
Fc.init
