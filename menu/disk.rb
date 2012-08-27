class Disk
  def self.menu
    Tree.quote `df -h`
  end
end
