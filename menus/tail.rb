class Tail
  #   def self.menu *args

  def self.ongoing
    dir = Tree.dir :file=>1
    Console.run "tail -f \"#{dir}\""
  end

  def self.in
    dir = Tree.dir :file=>1
    Tree.quote `tail "#{dir}"`
  end

  def self.menu_before *args
    # Don't do anything unless it's just numeric args
    return nil if args.length != 1 || args[0] !~ /^\d+$/

    dir = Tree.dir :file=>1

    count = args[0].to_i
    Tree.quote `tail -n #{count} "#{dir}"`
  end

  def self.menu
    "
    - .in/
    - 3/
    - .ongoing/
    - .docs/
      > Shows the first 10 lines inline
      << in/

      > Show N lines inline
      << 3/

      > Opens a shell and keeps showing updates
      << ongoing/
    "
  end

end
