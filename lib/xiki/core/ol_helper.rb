module Xiki
  class OlHelper

    # Finds last instance of file:line in /tmp/out_ol.xiki.lines,
    # and returns how far from the bottom of out_ol.xiki.lines it is.
    # OlHelper.source_to_output "/projects/foo/bah.rb", 6
    #   17
    def self.source_to_output file, line
      target = "#{file}:#{line}\n"

      lines = IO.readlines("#{Ol.file_path}.lines").reverse

      # Get how far from bottom
      lines_total = lines.length
      distance, found = 0, nil
      while distance < lines_total do
        break found = distance if lines[distance] == target
        distance += 1
      end

      found
    end

    # Expand while in ol buffer, so figure out where to navigate to
    def self.launch
      prefix = Keys.prefix :clear=>1

      # Get path from end
      path = Ol.file_path

      # Get total_lines - current_line
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

      Launcher.launch
    end

    def self.highlight_executed_lines

      prefix = Keys.prefix :clear=>1

      # 1. Get contents from Ol view
      orig = View.name
      View.to_buffer "ol"
      txt = View.txt.split("\n")
      View.to_buffer orig
      return if txt.length == 0

      # 2. Get equal number of lines from ....lines file
      count = txt.length
      paths = IO.readlines("#{Ol.file_path}.lines")
      paths = paths[-(count)..-1]



      # Dash+do+highlight, so jump to "continue here" line first

      if prefix == :u

        found = txt.index{|o| o =~ /\bcontinue here\b/}
        if found
          found = paths[found]
          path, number = found.strip.split(":")   #> 3
          View.open path
          View.line = number
          View.recenter
        end
      end

      # Clear existing highlights
      Color.clear_light


      # 3. For each line
      # target, orig_line = View.file, View.line
      target, orig_location = View.file, Location.new

      paths.each_with_index do |path, i|
        path, number = path.strip.split(":")
        next if path != target
        # Path matches current file

        # Ignore |... lines (only top line for each Ol.a output starts with a label)
        next if txt[i] =~ /^ *\|/

        View.open path
        View.line = number
        Color.mark "gray"

        value = txt[i].sub(/.+?\) ?/, "")

        # No useful value returned, so don't update comment
        # Nothing after "label)", so skip
        next if value.blank?

        # Literal 'Ol "..."' line without "#{", so skip
        line_value = Line.value
        next if line_value =~ /^[ |]*Ol "[^"]+"$/ && line_value !~ /#\{/

        # remove ") "
        # return nothing if > no ": "
        # remove "foo: "
        value.sub! /.+?: /, ''

        Ol.update_value_comment value
        # Jump to line and highlight
        # Also paste new value

      end

      View.open target
      orig_location.go

    end


  end
end
