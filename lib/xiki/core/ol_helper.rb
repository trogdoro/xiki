module Xiki
  class OlHelper

    # Finds last instance of file:line in /tmp/out_ol.notes.lines
    def self.source_to_output file, line
      target = "#{file}:#{line}\n"

      lines = IO.readlines("/tmp/out_ol.notes.lines").reverse

      lines_total = lines.length

      # Get how far from bottom
      distance, found = 0, nil
      while distance < lines_total do
        break found = distance if lines[distance] == target
        distance += 1
      end

      found
    end

    def self.launch
      prefix = Keys.prefix :clear=>1

      # Get path from end
      path = Ol.file_path #[/\/.+/]

      # TODO: get total_lines - current_line
      distance_to_end = Line.number(View.bottom) - Line.number

      # Go to log.lines and get n from end
      arr = IO.readlines("#{path}.lines")
      line = arr[- distance_to_end]

      path, line = line.split(':')


      if ! File.exists? path
        View.flash "- file \"#{path}\" not found!"
        raise "- file \"#{path}\" not found!"   # This throws an error, but at least blocks it from opening a blank file
      end

      View.open path
      View.to_line line.to_i

      if prefix == 0
        View.layout_outlog
        Line.next
      end
    end

    def self.open_last_outlog
      prefix = Keys.prefix :clear=>1
      View.layout_outlog
      if prefix == :u
        View.to_highest
        Search.forward "^-"
      else
        View.to_bottom
        Line.previous   # <= 1
      end

      Launcher.launch_unified
    end

end; end
