# Remember something you're searching for, and jump to the next occurrences.
  CODE_SAMPLES = %q<
    - Deprecated:
  >

class SearchTerm
  extend ElMixin
  @@last_dash_search = ""
  def self.keys
    define_key :isearch_mode_map, kbd("C--") do  # During isearch, remember current search term
      isearch_done
      isearch_clean_overlays
      match = buffer_substring match_beginning(0), match_end(0)
      @@last_dash_search = match
    end
    Keys.set("C--") do
      started = point
      chars = @@last_dash_search.size
      # Go to it
      search_forward @@last_dash_search
      # Go forward again if you were already at it
      search_forward @@last_dash_search if started == point - chars
      # Back up to beginning
      backward_char chars
    end
  end
  # Does an isearch on the characters typed, until the user pauses more than .4 seconds
  def self.timed_search

#    isearch_forward
    c = read_char("search: ")
#    keys = self.to_letter(keys)
    call_interactively key_binding c
    while(c = read_char("Input: ", nil, 0.40))
      call_interactively key_binding c
    end

  end
end
# Test stuff to search for, and stuff
