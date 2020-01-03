module Xiki

  class Tasks
    def self.enter_after

      prefix = Keys.prefix :clear=>1
      prefix = 1 if ! prefix.is_a? Fixnum

      orig = Location.new

      View.open "%n"
      todo_orig = Location.new
      View.to_highest

      prefix.times do
        Search.forward "^ *:\\+"
      end

      line = Line.value

      line.sub! /^ *:\+/, ''

      todo_orig.go
      orig.go

      View.insert line

    end

    def self.enter_upper

      prefix = Keys.prefix :clear=>1
      orig = Location.new

      View.open "%n"
      todo_orig = Location.new
      View.to_highest

      # Borrow here!
      View.line = prefix if prefix.is_a? Fixnum

      line = Line.value
      if prefix == :u || prefix == :uu
        lines_to_delete = 1
        Line.delete

        in_todo = Bookmarks['%n'] == orig.file
        orig.line -= 1 if in_todo   # If in :t, adjust position by how much is deleted

        # If blank line after, delete it
        if Line.blank?
          Line.delete
          orig.line -= 1 if in_todo   # If in :t, adjust position by how much is deleted
        end
      end

      todo_orig.go
      orig.go

      line << "\n" if prefix == :uu

      View.insert line
    end

    def self.enter_task
      txt = Line.blank? ? "xiki/" : "= xiki/"
      Tree << txt
      Launcher.launch
    end

    def self.hop_task
      # Get task on line
      task = Line.value
      # Open it in new view
      Launcher.open(task)
    end

  end
end
