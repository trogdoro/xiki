module Xiki
  class Layout
    def self.menu
      %`
      | Keys shortcuts that start with "to+" (ctrl-t) such as to+end.
      - frequent/
        | layout+create: Create a new view
        | layout+hide: Hide this view
        | layout+next: Go to next view
        | layout+previous: Go to previous view
        | layout+kill: Close the current file
      - all/
        | TODO
      |
      > See also
      << view/
      `
    end
  end
end
