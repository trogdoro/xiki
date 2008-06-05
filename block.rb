class Block
  extend ElMixin
  def self.value
    res = []
    with(:save_excursion) do
      found = re_search_backward "^ *$", nil, 1
      if found
        end_of_line
        forward_char
      end
      res << point
      re_search_forward "^ *$", nil, 1
      beginning_of_line
      res << point
    end
  end
end
