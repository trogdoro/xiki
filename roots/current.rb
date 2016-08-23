class Current
  def self.menu *name
    return "<< views/"
    Buffers.current *name
  end
end
