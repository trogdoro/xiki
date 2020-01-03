module Xiki
  class Hint

    def self.top_box txt

      txt = txt.strip

      # Make blank space at top

      height = txt.scan(/\n/).length + 4

      Move.top
      View << "\n" * (height+1)
      Move.to_end

      View.refresh
      $el.sit_for 0.3


      # 2. Draw green box, and fade in
      Move.top
      View.delete 1, (height+2)

      View << "\n#{txt.gsub(/^./, "  \\0")}\n\n                     (type any key)\n\n"

      right = View.cursor-2
      right = Line.left -1
      Overlay.face :diff_green, :left=>1, :right=>right

      View.line = height
      Overlay.face :fade3, :left=>Line.left, :right=>Line.right

      View.line = height+2

      Move.to_end

      # Key pressed, so hide it

      key = Keys.input :timed=>1, :raw=>1, :prompt=>" "

      # Delete it first
      Line.to_start
      View.delete 1, View.cursor

      View << "\n" * (height+1)
      Move.to_end
      View.refresh
      $el.sit_for 0.3

      # Delete lines, one at a time
      (height+1).times do
        View.delete 1, 2
        View.refresh
        $el.sit_for 0.03
      end
      Files.revert
      View.message ""

      Move.to_end

      key   # Return the key they pressed

    end
  end
end
