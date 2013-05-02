class Poro
  def self.aa *args
    "x #{args}"
  end
  def self.bb
    "bbbbbbb"
  end
  def self.set_z arg=nil
    return "sample" if !arg
    @z = arg
    "- set it to #{arg}!"
  end
  def self.get_z
    @z
  end
end
