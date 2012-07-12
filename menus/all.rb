class All
  def self.menu
    Launcher.menu_keys.map{|o| "<< #{o}/"}.join("\n")
  end
end
