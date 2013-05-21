module Xiki
  class Open
    def self.menu
      %`
      > Summary
      | Keys shortcuts that start with "to+" (ctrl-t) such as to+end.
      |
      - frequent/
        > Jumping cursor around
        | open+bookmark: jump to a file
        | open+tree: view a tree of a directory
        | open+current: shows currently open files
        | open+edited: shows recently edited files
        | open+menu: opens view that lets you type a menu (type "-" to see all)
        |
      - others/
        | open+history: shows recently viewed files
      `
    end
  end
end
