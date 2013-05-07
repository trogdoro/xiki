class Words
  def self.menu *args

    # If no args, explain what it does
    if args.empty?
      return View.prompt "Add some text to count the words."
    end

    txt = ENV['txt'].scan(/[a-z']+/i).length.to_s
  end
end
