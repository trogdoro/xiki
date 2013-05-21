require 'xiki/core/view'
require 'xiki/core/line'
require 'xiki/core/effects'


class Agenda

  def self.menu *lines
    t = Bookmarks['$t']

    # If no line, display all of them
    if lines.empty?
      return IO.read(t).grep(/^> \d\d\d\d-\d\d-\d\d: /).sort.reverse.join("").gsub(/^> /, '- ')
    end

    line = Line.value

    # If line, jump to it in $t
    find = Line.value.sub /^[ -]+/, ''
    View.open t
    View.to_top
    Search.forward "^. #{$el.regexp_quote(find)}"
    Line.to_left
    View.recenter_top
    Effects.blink(:what=>:line)

  end
end
