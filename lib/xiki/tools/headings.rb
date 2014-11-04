require 'xiki/core/view'
require 'xiki/core/line'
require 'xiki/core/effects'

module Xiki
  class Headings

    def self.menu target=nil, line=nil
      todo = Bookmarks[':t']

      # If no target, tell them to enter one
      if target.nil?
        return View.prompt "Type a heading to search for"
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
end
