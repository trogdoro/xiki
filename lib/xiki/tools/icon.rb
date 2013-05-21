module Xiki
  class Icon
    def self.menu name=nil, choice=nil

      name = choice if name == "recent" && choice

      # If they picked recent/, just list icons

      return Launcher.last "", :omit=>'icon' if name == "recent"

      # If nothing, show main options

      if name.nil?

        return "
          > Summary
          | Makes an icon in the OS that opens a xiki menu.
          | Does this work currently?
          - type menu name here) 
          - recent/
          << see also) xiki/setup/install icon/
          "
      end

      # Name passed so create menu

      dir = Tree.dir :or=>:desktop

      path = "#{dir}/#{name}.xiki"
      File.open(path, "w") { |f| f << "\n" }
      Files.open_in_window path

      "- created '#{name}' icon!"
    end
  end
end
