class Source
  def self.menu *args

    path = yield[:ancestors]
    return "
      : Nest this under a menu name, like this:
      =ip/
        =source/
      :
      <= options/
      <= echo/
      " if ! path

    options = Expander.expanders path
    return "- no sources found!" if !options[:sources]

    Command.climb_sources options
    sources = options[:sources]

    result = "=#{options[:menufied].sub /(.+\/).+/, "\\1"}\n"

    sources.each_with_index do |l, i|
      l.reverse.each do |file|
        result << "  #{'  ' * i}+ #{file}\n"
      end
    end

    result
  end
end
