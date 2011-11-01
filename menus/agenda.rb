require 'view'
require 'line'
require 'effects'


class Agenda

  def self.menu line=nil
    t = Bookmarks['$t']

    # If no line, display all of them
    if line.nil?
      return IO.read(t).grep(/^[>|] \d\d\d\d-\d\d-\d\d/).sort.join("").gsub(/^[>|] /, '- ')
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
