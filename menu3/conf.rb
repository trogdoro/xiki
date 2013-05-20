class Conf

  # Use notes styles for .conf files
  def self.init
    Mode.define(:conf, ".conf") do
      Notes.mode
    end
  end

end
Conf.init   # Define mode
