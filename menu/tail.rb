class Tail

  def self.ongoing
    dir = Tree.dir :file=>1
    Console.run "tail -f \"#{dir}\""
  end

  def self.dir
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

  MENU = "
    - .ongoing/
    - 3/
    - .dir/
    - .docs/
      > Shows the first 10 lines inline
      << in/

      > Show N lines inline
      << 3/

      > Opens a shell and keeps showing updates
      << ongoing/
    "

end
