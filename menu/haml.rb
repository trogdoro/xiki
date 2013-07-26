module Xiki::Menu
  class Haml
    def self.menu *args

      return "
        > Type some HAML here to run it in your browser
        | %h1 Example
        | %table{style: 'width:100%;'}
        |   %tr
        |     %td Stuff in
        |     %td Table cells
        " if args == []

      txt = args[0]
      return "@beg/quoted/" if txt !~ /\n/


      File.open("/tmp/tmp.haml", "w") { |f| f << txt }
      output = Console.sync "haml /tmp/tmp.haml /tmp/tmp.html"

      if output.any?
        return Tree.quote output
      end

      File.open("/tmp/tmp.html", "a") { |f| f << Xiki::Html.default_css }

      # Then load in browser (or reload)
      Firefox.value('document.location.toString()') == "file:///tmp/tmp.html" ?
        Firefox.reload :
        $el.browse_url("file:///tmp/tmp.html")

      "@flash/- Loaded in browser!"
    end
end; end
