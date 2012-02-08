class Icon
  def self.menu name=nil, choice=nil
Ol << "name: #{name.inspect}"

    name = choice if name == "recent" && choice

    # If they picked recent/, just list icons

    return Launcher.last "", :omit=>'icon' if name == "recent"

    # If nothing, show main options

    if name.nil?
      last = Launcher.last "", :omit=>'icon'
      most_recent = last[/^- (.+?)\//, 1]

      return "
        | Make an icon for which menu?  Use one of these options:
        - type menu name here) #{most_recent}
        - recent/
        "
    end

    # Name passed so create menu


    dir = Tree.dir :or=>:desktop
    #     dir = Tree.dir || File.expand_path("~/Desktop")
Ol << "dir: #{dir.inspect}"

    path = "#{dir}/#{name}.xiki"
    File.open(path, "w") { |f| f << "\n" }
    Files.open_in_window path

Ol << "!"
    #     ".flash - created '#{name}' icon!"
    "- created '#{name}' icon!"

  end
end
