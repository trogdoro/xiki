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

    View.kill if View.name == "@dimensions/"

    nil
  end
end
