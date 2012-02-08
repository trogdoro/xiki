class Info
  def self.menu *args

    dir = Tree.dir :file=>1

    result = `ls -lah "#{dir}"`
    Tree.quote result
  end
end
