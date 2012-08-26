class WebServer


  def self.index
    no_keys = false

    if ENV['REQUEST_METHOD'] == "POST"
      cgi = CGI.new
      post_txt = cgi['txt']

      # Temporary hack
      File.open("/tmp/post_tmp", "w") { |f| f << post_txt }

      # What's a better way to do this?
      # Pass in pipe, and send to shell command via STDIN

    end

    menu = ENV['QUERY_STRING']

    return self.usage if menu == ""

    # Run command...

    command = "xiki #{menu}"

    txt = `#{command}`

    # TODO: return different string when the service is down

    if txt.empty?
      menu = menu.sub /\/$/, ''

      no_keys = true

      if menu =~ /\/./   # If slash that's not at end
        puts "<pre>Nothing returned.  Maybe service is down, or maybe menu\njust returned nothing, run xiki command\n\n  $ xiki\n"
      else
        puts %`
          <h1>Menu '#{menu}' doesn't exist yet.  Create it?</h1>

          <form action="/create/#{menu}" method="post" id="as_menu">
          <div class='toggle'>
            <span>as text</span>
            | <a href="#" onclick="$('#as_class, #as_menu').toggle(); return false;">as class</a>
          </div>
          <textarea name="txt">
          - Sample item/
            - Another sample item/
          - Yet another/
          </textarea>
          <br>
          <input type="submit" value="save" class="save">
          </form>

          <form action="/create/#{menu}" method="post" id="as_class" style="display:none;">
          <div class='toggle'>
            <a href="#" onclick="$('#as_class, #as_menu').toggle(); return false;">as text</a>
            | <span>as class</span>
          </div>
          <textarea name="txt">
          class #{self.camel_case menu}
            def self.menu *args
              "Menu was called, with params \#{args.inspect}"
            end
          end
          </textarea>
          <br>
          <input type="submit" value="save" class="save">
          </form>

          `.gsub(/^          /, '')
      end
    end

    # Show this again when I figure out how to recognize server being down!

    #     if txt.empty?
    #       puts "<pre>Command received no output.  Possibly the xiki process
    #         isn't running. Run the 'xiki' command in a shell:

    #           $ xiki

    #         </pre>".gsub(/^        /, '')
    #     end

    # Html-ify and print output...

    txt = self.htmlify txt, :no_keys=>no_keys
    print txt

  rescue Exception=>e
    puts "<pre>#{e.message}\n#{e.backtrace}</pre>"
  end

  def self.camel_case txt
    return txt if txt !~ /_/ && txt =~ /[A-Z]+.*/
    txt.split('_').map{|e| e.capitalize}.join
  end

end
