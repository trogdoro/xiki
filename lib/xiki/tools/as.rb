module Xiki
  class As
    def self.menu
      %`
      > Summary
      | Keys shortcuts that start with "to+" (ctrl-t) such as to+end.
      |
      - frequent/
        | as+clipboard: Copy (after doing Control-space on the other side)
        | as+kill: Cut (after doing Control-space on the other side)
        | as+bookmark: remember this file as a bookmark
      - others/
        | TODO
      `
    end
  end
end
