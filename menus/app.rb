class App

  def self.menu *name

    return View.prompt("Type a mac app to open", :times=>4) if name.empty?

    name = name.join "/"

    # Open app

    command = "open \"/Applications/#{name}.app\""
    Console.sync command

  end
end
