# Helps you to insert "1", "2", "3"....   Useful during macros.
class Incrementer
  def self.start
    Clipboard['n'] = (Keys.prefix_n || 0).to_s
  end

  def self.increment
    Clipboard['n'] = Clipboard['n'].next
  end

  def self.enter
    self.increment if Keys.prefix_u
    View.insert Clipboard['n']
  end
end
