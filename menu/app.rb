#
# Shows each OS X app, and lets you launch them.
#
class App
  def self.menu name=nil

    if ! name
      txt = Dir.new("/Applications/").entries.select{|o| o =~ /\.app$/}.map{|o| "#{o[/(.+)\.app$/, 1]}/"}
      return txt
    end

    Console.sync "open \"/Applications/#{name}.app\""
    nil
  end
end
