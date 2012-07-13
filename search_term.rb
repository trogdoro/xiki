# Remember something you're searching for, and jump to the next occurrences.
  CODE_SAMPLES = %q<
    - Deprecated:
  >

class SearchTerm

  @@last_dash_search = ""
  def self.keys
    $el.define_key :isearch_mode_map, $el.kbd("C--") do  # During isearch, remember current search term
      $el.isearch_done
      $el.isearch_clean_overlays
      match = $el.buffer_substring $el.match_beginning(0), $el.match_end(0)
      @@last_dash_search = match
    end
    Keys.set("C--") do
      started = $el.point
      chars = @@last_dash_search.size
      # Go to it
      $el.search_forward @@last_dash_search
      # Go forward again if you were already at it
      $el.search_forward @@last_dash_search if started == $el.point - chars
      # Back up to beginning
      $el.backward_char chars
    end
  end
  # Does an isearch on the characters typed, until the user pauses more than .4 seconds
  def self.timed_search

#    isearch_forward
    c = $el.read_char "search: "
#    keys = self.to_letter(keys)
    $el.call_interactively $el.key_binding(c)
    while(c = $el.read_char("Input: ", nil, 0.40))
      $el.call_interactively $el.key_binding(c)
    end

  end
end
# Test stuff to search for, and stuff
