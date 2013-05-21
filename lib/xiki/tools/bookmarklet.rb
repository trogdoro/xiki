module Xiki
  class Bookmarklet
    def self.menu

      %`
      | // Customize this js code and then click "create"
      | alert('hi')
      - .create/
      - docs/
        > Summary
        | Use this menu to create bookmarklets. A bookmarklet is a bookmark (in
        | a web browser) that runs javascript code to do something interesting.
        |
        | After you modify the javascript (which can be multiple lines)
        | and click "create", you will see instructions in the browser.
        |
        > jQuery
        | The generated bookmarklet will automatically reqire jQuery if your
        | script contains the string "$(" anywhere.
      `
    end

    def self.create name=nil, options={}

      if name.nil?
        return View.prompt "Give your bookmarklet a name"
      end

      txt = Tree.siblings :before=>1
      txt = txt.select{|o| o =~ /^\|/}   # We only want quoted ones

      txt = txt.join("\n").gsub(/^\| ?/, '')

      txt.gsub! '"', "%22"
      txt.gsub!(/(^| +)\/\/.+/, "")

      if txt =~ /\$\(/   # If it uses the $ var, include jquery
        txt = Javascript.wrap_jquery_load txt
      end

      html = %`
        <title>Bookmarklet</title>
        <p>Click the link to try it out.  Then drag the link to your toolbar to create a bookmarklet:</p>

        <a href="javascript:
        (function(){
        #{txt}
        })()
        ">#{name}</a>
        `.unindent

      File.open("/tmp/bookmarklet.html", "w") { |f| f << html }

      Firefox.url "file:///tmp/bookmarklet.html"

      Applescript.run "Firefox", "activate"

      "
      | Go to the browser to save the boomarklet
      "

    end

  end
end
