module Xiki
  class LocalStorage
    def self.menu
      "
      - .all/
      - docs/
        > Summary
        | View and manipulate records in the browser's \"Local Storage\" database.
        |
        > Show all records
        << all/
        |
        > Manipulating records
        | save: double-click record contents
        | delete: do+kill+it while on the record (Ctrl-d Ctrl-k Ctrl-i)
        |
        > Delete all keys
        @ js/localStorage.clear()
      "
    end

    def self.all key=nil, val=nil
      prefix = Keys.prefix :clear=>true


      if key.nil?   # If nothing passed, show all keys
        js = %`
          var result = [];
          for (var key in localStorage) { result.push(key) }
          JSON.stringify(result);
          `
        txt = Firefox.exec js   #:jquery=>1  #, :jquery_extra=>ls_function
        array = JSON[txt]
        return "
          | None found.  Create a few test records?
          - a/aa
          - b/bb
          " if array.empty?
        return array.sort.inject("") {|acc, element| "#{acc}- #{element}/\n"}
      end

      if Keys.delete?
        Firefox.exec "localStorage.removeItem(\"#{key}\")"
        Tree.to_parent if val
        Tree.kill_under
        View.flash "- deleted!"
        # TODO: let this behave properly when returning with .flash:
        # Currently it ends up one line off?  Because we're treating it like it returned nil - make it treat ^.flash as nill?
        # return ".flash - deleted!"
        Line.delete
        return
      end

      if val.nil?   # If just key passed, show val
        txt = Firefox.exec "localStorage[\"#{key}\"]"
        return self.docs if txt.blank? && key =~ /docs\/?/
        return self.api if txt.blank? && key =~ /api\/?/
        return Tree.quote txt
      end

      Tree.unquote! val

      val = ENV['txt']
      txt = Firefox.exec "localStorage[\"#{key}\"] = #{val.inspect}"
      View.flash "- Saved!"
    end

  end
end
