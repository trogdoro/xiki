class Itunes
  def self.menu
    "
    - .play/
    - .pause/
    - .next/
    - .previous/
    - .artists/
    - .songs/
    - .playlist/
    - api/
      > Play a song
      @ Itunes.songs 'Thunderstorm'
    - docs/
      | Play specific song
      @itunes/songs/Around the Fur
      |
      | Play a playlist
      @itunes/playlist/class
    "
  end

  def self.songs name=nil

    # If nothing passed, list all songs
    if name.nil?
      tracks = Applescript.run "iTunes", "get the name of every track of library playlist 1"
      tracks = JSON[tracks.sub(/^\{(.+)\}$/, "[\\1]")]
      return tracks.sort.uniq.select{|o| o != "" && o !~ /^ /}.map{|o| "- #{o}/\n"}.join
      return
    end

    Applescript.run "iTunes", "play track \"#{name}\""

  end

  def self.playlist name=nil
    return View.prompt("Enter a name") if name.nil?
    Applescript.run "iTunes", "play playlist \"#{name}\""
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

  def self.artists artist=nil, track=nil

    # If nothing passed, list artists

    if artist.nil?
      artists = Applescript.run "iTunes", 'get the artist of every track of library playlist 1'
      artists = JSON[artists.sub(/^\{(.+)\}$/, "[\\1]")]
      return artists.sort.uniq.select{|o| o != ""}.map{|o| "- #{o}/\n"}.join
    end

    # If just artist passed, list artists

    if track.nil?
      tracks = Applescript.run "iTunes", "get the name of every track of library playlist 1 whose artist is \"#{artist}\""
      tracks = JSON[tracks.sub(/^\{(.+)\}$/, "[\\1]")]
      return tracks.sort.uniq.select{|o| o != ""}.map{|o| "- #{o}/\n"}.join
    end

    self.songs track
    ".flash - Playing!"
  end
end
