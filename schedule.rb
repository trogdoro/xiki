class Schedule
  def self.menu line=nil

    t = Bookmarks['$t']

    # If no line, display all of them
    if line.nil?
      return IO.read(t).grep(/^\| e /).join("").gsub(/^\| e/, '-')
    end

    # If line, jump to it in $t
    find = Line.value.sub /^[ -]+/, '| e '
    View.open t
    View.to_top
    Search.forward "^#{$el.regexp_quote(find)}"
    Line.to_left
    Effects.blink(:what=>:line)
    View.recenter_top

  end
end
