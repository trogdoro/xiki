class XikiPath
  def self.menu *args
    dirs = Xiki.xiki_path_dirs
    dirs.map{|o| "= #{o.sub /\/*$/, '/'}"}.join("\n")
  end
end
