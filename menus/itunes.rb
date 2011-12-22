class Itunes
  def self.menu
    "
    - .next/
    - .previous/
    - .play/
    - .pause/
    - .artists/
    "
  end

  def self.play
    Applescript.run "iTunes", "play"
  end
  def self.pause
    Applescript.run "iTunes", "pause"
  end

  def self.next
    Applescript.run "iTunes", "next track"
  end
  def self.previous
    Applescript.run "iTunes", "previous track"
  end

  def self.artists
    artists = Applescript.run 'tell application "iTunes" to get the artist of every track of library playlist 1'
    artists = JSON[artists.sub(/^\{(.+)\}$/, "[\\1]")]
    artists.sort.uniq.select{|o| o != ""}.map{|o| "- #{o}/\n"}.join
  end
end
