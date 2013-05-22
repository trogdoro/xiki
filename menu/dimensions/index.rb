class Dimensions
  def self.menu_before item=nil

    return if ! item

    # An item exists, so eval it if it's in the users dimensions.conf...

    # Otherwise .expand would just look at index.menu
    conf = yield[:conf]
    txt = Tree.children conf, item

    return if ! txt

    # Eval output...

    code = txt.gsub /^! ?/, ''
    returned, out, exception = Code.eval code # , source_file, 1

    View.kill if View.name == "@dimensions/"

    ""
  end

  def self.menu_after output, *args

    # /, so prepend user conf if there, and append @conf...

    if args.blank?
      conf = yield[:conf]
      if conf   # If conf, add item names defined by user
        conf.gsub! /^-/, '+'
        items = conf.scan(/^[+-] .+/)   # Just pull out items
        items += output.split "\n"   # Split into arrays and merge, so we can remove duplicates
        items.uniq!
        output = items.join "\n"
      end

      return "#{output}\n@conf/"
    end


    # Close view if we opened it with a shortcut...

    # TODO: We can probably make this generic
    # Know to close after the first launch (that returns nil?) when
    #   - Maybe always close when :letter param
    #     - maybe only close when :letter=>"close"
    #     - yes, probably do this
    #       - just make the code that got the letter input close the view
    #         - when the view name is @...
    #         - then there's no need for this file
    #   - or possibly: make name of view special
    #     - to indicate that it should be closed
    #       - probably isn't necessary, since :letter-related code will still be running

    # /projects/xiki/lib/xiki/
    #   - key_bindings.rb
    #     |     Xiki.def "layout+dimensions", "dimensions/", :letter=>1

    View.kill if View.name == "@dimensions/"


    nil
  end
end
