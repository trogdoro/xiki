require 'auto_menu'

class Projects

  @@projects ||= {'default'=>'$tr'}

  def self.menu
    [@@projects.map {|k,v| "#{k}: #{v}/"}, '.edit/'].flatten.sort.map{|l| "#{l}"}
  end

  def self.edit
    puts "
      - To edit the listing of projects, add a line like this:
        ~/.el4r/
          init.rb
            | Projects.listing = {:blog => '$tr', :foo => '/projects/foo'}
        - The strings correspond to paths or bookmarks
      "
  end

  def self.listing= the_listing
    @@projects = the_listing
  end

  def self.listing
    @@projects
  end
end
