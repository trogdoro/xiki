#
# Shows each OS X app, and lets you launch them.
#
class App
  def self.menu name=nil

    if ! name
      txt = Dir.new("/Applications/").entries.select{|o| o !~ /^\./}.map{|o| "#{o[/(.+)\.app$/, 1]}/"}
      return txt
    end

    command = "open \"/Applications/#{name}.app\""
    Console.sync command
    nil
  end
end
