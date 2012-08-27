class Specials
  def self.menu *args
    dir = Tree.dir :file=>1

    `od -ct uC #{dir}`.gsub(/^/, '| ').gsub(/ +$/, '')
  end
end
