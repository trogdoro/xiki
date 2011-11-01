require 'view'
require 'line'
require 'effects'

class Headings

  def self.menu target=nil, line=nil
    todo = Bookmarks['$t']

    # If no target, tell them to enter one
    if target.nil?
      Move.to_end
      View.message "Type something to search for in todo.notes headings."
      return nil
    end

    # If no line, search for headings
    if line.nil?
      txt = IO.read(todo)
      sections = txt.split(/^> /, -1)
      sections = sections.select{|o|
        # Check just first line
        o[/.*/] =~ /#{Regexp.quote target}/i
      }
      return sections.map{|o| "> #{o.strip}\n\n"}.join("").gsub(/^/, '| ').gsub(/^\| $/, '|')
    end

    # Launched quoted line, so jump to it

    line = Line.value

    find = line.sub /^  \| /, ''
    View.open todo
    View.to_top
    Search.forward "^#{$el.regexp_quote(find)}$"
    Line.to_left
    View.recenter_top
    Effects.blink(:what=>:line)

  end
end
