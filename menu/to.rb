class To
  def self.menu
    %`
    > Summary
    | Keys shortcuts that start with "to+" (ctrl-t) such as to+end.
    |
    - frequent/
      > Jumping cursor around
      | to+row - jump to line
      | to+axis - beginning of line
      | to+end - end of line
      |
      | to+previous - beginning of previous paragraph
      | to+next - beginning of next paragraph
      |
      | to+highest - beginning of file
      | to+lowest - end of file
      |
    - others/
      | to+backward - backward one word
      | to+foreward - foreward one word
    `
  end
end
