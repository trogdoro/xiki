class All
  def self.menu
    Launcher.menu_keys.map{|o| "<< #{o.gsub '_', ' '}/"}.join("\n")
  end
end
