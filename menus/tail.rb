class Tail
  def self.menu *args
Ol << "args: #{args.inspect}"

    count = args.shift if args[0] =~ /^\d+$/

    # If nothing passed (except maybe number of lines), do tail

    if args.blank?
      dir = Tree.dir :file=>1
      command = "tail #{count ? "-n #{count}" : ""} \"#{dir}\""
      Tree.quote Console.sync(command)
      return
    end

    Ol << "TODO passed line, so navigate to it!"

  end
end
