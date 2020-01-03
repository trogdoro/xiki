class Dimensions

  def self.menu_before *args

    options = yield
    prefix = options[:prefix]

    conf_file = File.expand_path "~/.xiki/roots/conf/dimensions.conf"

    options[:no_search] = 1 if args[0] == "current"

    # as+open...

    if prefix == "open"

      item = args[-1]

      # It's open+...

      raise "- dimensions.conf doesn't exist yet!" if ! File.exists? conf_file
      View.open conf_file, :to=>"^[ +-]*#{$el.regexp_quote item}/?$"  # "

      return ""
    end

    # enter+all...

    if prefix == "all"   # enter+all
      options[:no_search] = 1
      return Tree.children File.read(conf_file), args
    end

    # if !..., eval it, or open

    if args[-1] =~ /^! /

      # If !..., we want all the lines
      return "=beg/neighbors/" if args[-1] !~ /\n/

      if prefix == "update"   # as+open
        self.update conf_file, *args
        return ""
      end

      code = args[-1]
      code.gsub! /^! /, ''
      eval args[-1]
      return ""
    end

    nil   # Don't interject
  end

  def self.update conf_file, name, code
    txt = File.read conf_file
    txt = Tree.update txt, [name, code.strip]
    File.open(conf_file, "w") { |f| f << txt }
  end

  def self.menu_after output, *args

Ol "output", output
    # /, so prepend user conf if there, and append =conf...

    if args.blank?
      conf = yield[:conf]
      if conf   # If conf, add item names defined by user
        # conf.gsub! /^-/, '+'
        items = conf.scan(/^[+-] .+/)   # Just pull out items
        items += output.split "\n"   # Split into arrays and merge, so we can remove duplicates
        items.uniq!
        output = items.join "\n"
      end

      return "#{output}\n=conf/"
    end

    View.kill(:force_recent=>1) if View.name == "dimensions/" && args != ["current"]

    # Run it through =conf output if matched...

    # output is "" if it used a built-in item

    if ! output
      conf = yield[:conf]
      txt = Tree.children conf, args
      MenuHandler.eval_exclamations txt
      return txt
    end

    # Do nothing
    nil
  end
end
