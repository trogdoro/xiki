class Layout
  def self.menu
    %`
    > Summary
    | Keys shortcuts that start with "to+" (ctrl-t) such as to+end.
    |
    - frequent/
      | layout+create: Create a new view
      | layout+hide: Hide this view
      | layout+next: Go to next view
      | layout+previous: Go to previous view
      | layout+kill: Close the current file
    - others/
      | TODO
    `
  end
end
