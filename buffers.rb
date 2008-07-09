class Buffers
  def self.menu buffer=nil

#return $el.ml buffer
    if buffer == nil  # If no buffer, show list
      $el.buffer_list.map { |b| $el.buffer_name(b) }.to_a.each do |b|
        #name = $el.buffer_name(b)
        puts "+ #{b}"
      end
      return
    end

    # Switch to buffer
    View.to_after_bar if View.in_bar?
    View.to_buffer(buffer)

  end
end
