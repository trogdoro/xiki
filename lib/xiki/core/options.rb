class Xiki::Options
  def self.args options
    "args = #{(options[:args]||[]).inspect}" #.gsub(/=>#<.+?>/, "=>'proc'")
  end
end
