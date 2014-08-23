module Xiki
  class Buffers

    def self.menu # buffer=nil
      "
      - .current/
      - .tree/
        - 20/
      - .search/

      > Lists
      - .list/
      - .current/

      - docs/
        > Todo
      "
    end

    # Mapped to open+current and @current
    # Open list of buffers
    def self.current *name

      options = yield

      quoted = options[:dropdown] == ["quoted"]
      prefix = Keys.prefix :clear=>1

      # /, so show list of buffers...

      if name.empty?
        # Show all by default
        if ! prefix || prefix == :u || prefix == "all" || quoted
          result = Buffers.list.map do |b|
            name = $el.buffer_name(b)
            next if prefix != :u && name =~ /^\*/
            next if name =~ /^ \*/

            next if ["views/", "edited/", "*ol", "files.notes", "tasks.notes", "difflog.notes"].member?(name)
            next if quoted && (name =~ /^\*/ || ["edited/", "*ol", "files.notes", "tasks.notes", "difflog.notes"].member?(name))   # Skip the current buffer
            modified = $el.buffer_file_name(b) && $el.buffer_modified_p(b) ? "+" : " "

            # Use ":" if modified or name has crazy chars

            bullet = (modified == "+" || name =~ /[^a-z_.]/i) ? ":" : "-"

            txt = "#{bullet}#{modified}#{name}\n"

            # ~quoted, so go grab quote from buffer
            if quoted
              part = View.part(b, :context=>(prefix||2))
              txt << part.gsub(/^/, "  ")
            end
            txt
          end.join('')

          if quoted
            return "- none found" if result == ""

            options[:line_found] = result[/.*?^ +\|/m].count("\n")+1
            options[:no_search] = 1
          end
          return result
        end

        case prefix
        # Only files (no buffers)
        when :u
          return self.list.select{ |b| $el.buffer_file_name(b) }.map{ |b| ": #{$el.buffer_name(b)}\n" }.join('')

        # Only buffer without files
        when 0;
          return self.list.select{ |b| ! $el.buffer_file_name(b) }.map{ |b| ": #{$el.buffer_name(b)}\n" }[1..-1].join('')

          # Only files, already handled with :u
          #       when 1;  return self.list.select{ |b| $el.buffer_file_name(b) }.map{ |b| $el.buffer_name(b) }[1..-1]

        when 3;  return self.list.select{ |b| ! $el.buffer_file_name(b) && $el.buffer_name(b) =~ /^#/ }.map{ |b| $el.buffer_name(b) }
        when 4;  return self.list.select{ |b| ! $el.buffer_file_name(b) && $el.buffer_name(b) =~ /^\*console / }.map{ |b| $el.buffer_name(b) }
        when 6;  return self.list.select{ |b| $el.buffer_file_name(b) =~ /\.rb$/ }.map{ |b| $el.buffer_name(b) }
        when 7;  return self.list.select{ |b| $el.buffer_file_name(b) =~ /\.notes$/ }.map{ |b| $el.buffer_name(b) }

        end
        return
      end

      # /foo, so jump to or delete buffer...

      name[0].sub! /^\:./, ''

      dropdown = options[:dropdown]

      # Right-clicked, so show options
      return "~ close" if dropdown == []

      # If as+delete, just delete buffer, and line
      if dropdown == ["close"]
        Buffers.delete name[0]
        if name.length > 1
          Tree.to_parent
          Tree.kill_under
        end
        Line.delete
        return
      end

      return "<! not open" if ! View.buffer_open?(name[0])

      # Switch to buffer
      View.to_after_bar if View.in_bar?
      View.to_buffer(name[0])
    end

    def self.names_array
      self.list.map { |b| $el.buffer_name(b) }.to_a
    end

    def self.list
      $el.buffer_list.to_a
    end

    def self.tree times=0, options={}
      times ||= History.prefix_times
      paths = View.files[0..(times-1)]
      if options[:dir]
        paths = paths.grep(Regexp.new(Regexp.escape(options[:dir])))
      end
      puts CodeTree.tree_search_option + Tree.paths_to_tree(paths)
    end

    def self.search string, options={}

      orig = View.buffer

      # Get buffer from name
      list = options[:buffer] ?
        [self.from_string(options[:buffer])] :
        self.list
      found = ""

      list.to_a.each do |b|  # Each buffer open

        file = $el.buffer_file_name(b)
        #       file = $el.buffer_file_name(b) || "*#{View.name}"
        # Show buffers too - wasn't as simple as just removing, because of filename indenting!

        next unless file
        next if file =~ /_ol.notes/

        if options[:buffer].nil?   # If we're not searching in one buffer
          next if ["tasks.notes", "files.notes"].
            member? file.sub(/.+\//, '')
        end

        # Skip if a verboten file
        unless options[:buffer]
          next if file =~ /(\/difflog\.notes|\.log|\/\.emacs)$/
        end

        $el.set_buffer b
        started = $el.point
        View.to_top
        found_yet = nil
        while(true)
          break unless $el.search_forward(string, nil, true)
          unless found_yet
            found << "=#{file.sub(/(.+)\//, "\\1\/\n  - ")}\n"

            found_yet = true
          end

          found << "    : #{Line.value}\n"
          Line.end
        end
        View.to started
      end

      View.to_buffer orig

      # If nothing found, just insert message
      if found.size == 0
        Tree << "- nothing found!\n"
        Search.isearch string, :reverse=>1
        return
      end

      Tree << found
    end

    def self.from_string name
      $el.get_buffer name
    end

    def self.open_viewing
      case Keys.prefix
      when nil;  Launcher.open("- Buffers.tree 25/")
      when 0;  Launcher.open("- Buffers.tree/")
      else  Launcher.open("- Buffers.tree #{Keys.prefix}/")
      end
    end

    def self.rename
      options = {:prompt => "Rename buffer to: "}
      options[:initial_input] = $el.buffer_name if Keys.prefix_u?
      $el.rename_buffer Keys.input(options)
    end

    # Buffers.file View.buffer
    def self.file buffer
      $el.buffer_file_name buffer
    end

    def self.name buffer
      $el.buffer_name(buffer)
    end

    def self.kill name
      self.delete name
    end

    def self.delete name
      $el.kill_buffer name
    end

    def self.to name
      View.to_buffer name
    end

    # Return contents of a buffer
    def self.txt name
      $el.with(:save_window_excursion) do
        $el.switch_to_buffer name
        View.txt
      end
    end

  end
end
