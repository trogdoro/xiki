class Source
  def self.menu *args

    path = yield[:ancestors]
    raise "> How to use\n| Nest this under a menu name\n" if ! path

    options = Expander.expanders path
    return "- no sources found!" if !options[:sources]

    Menu.climb_sources options
    sources = options[:sources]

    result = "@#{options[:menufied].sub /(.+\/).+/, "\\1"}\n"

    sources.each_with_index do |l, i|
      l.reverse.each do |file|
        result << "  #{'  ' * i}+ #{file}\n"
      end
    end

    result
  end
end
