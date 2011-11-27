class Edited
  def self.menu buffer=nil
    paths = Files.edited_array[0..300]
    paths.map!{|i| i.sub(/(.+\/)(.+)/, "- @\\1\\2")}
    paths.join("\n")
  end
end
