class Matches
  def self.menu *args

    # If no args, explain what it does

    if args.empty?
      return View.prompt "Count matches of what?"
    end

    # If only one arg, have them add some text

    if args.length == 1
      return Tree << "| Add some text here to matches the matches."
    end

    txt = ENV['txt'].scan(/#{args[0]}/i).length.to_s
  end
end
