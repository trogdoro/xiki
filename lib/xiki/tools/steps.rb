module Xiki
  class Steps

    # Use notes styles for .steps files
    def self.init
      Mode.define(:steps, ".steps") do
        Notes.mode
      end
    end

  end
  Steps.init   # Define mode
end
