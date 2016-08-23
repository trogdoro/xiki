#
# Shows each OS X app, and lets you launch them.
#
class Applications
  def self.menu name=nil

    if ! name
      txt = Dir.new("/Applications/").entries.select{|o| o =~ /\.app$/}.map{|o| "#{o[/(.+)\.app$/, 1]}/"}
      return txt
    end

    Shell.sync "open \"/Applications/#{name}.app\""
    nil
  end
end
