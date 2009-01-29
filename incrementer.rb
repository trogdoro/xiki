# Helps you to insert "1", "2", "3"....   Useful during macros.
class Incrementer
  @@i = 1
  def self.start
    @@i = Keys.prefix_n || 0
  end
  def self.increment
    @@i += 1
  end
  def self.enter
    View.insert @@i.to_s
    self.increment if Keys.prefix_u
  end
end
