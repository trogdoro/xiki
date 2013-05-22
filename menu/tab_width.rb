class TabWidth
  def self.menu *args
    return "
      > Make tabs be this many spaces
      - 2/
      - 4/
      - 8/
      - 12/
      - 16/
      - 24/
      " if args.empty?

    $el.elvar.tab_width = args[0].to_i
    nil
  end
end
