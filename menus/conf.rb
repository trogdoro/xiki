class Conf
  def self.menu *args
    Launcher.menu_keys.select{|o| o =~ /config/}.map{|o| "<< #{o}/"}.join("\n")
  end
end
