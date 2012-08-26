class Line
  def self.number
    $curbuf.line_number
  end
  def self.value
    $curbuf.line
  end
end
