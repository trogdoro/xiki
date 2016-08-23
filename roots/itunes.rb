module Xiki
  class Itunes
    MENU = "
      - .play/
      - .pause/
      - .next/
      - .previous/
      - .artists/
      - .songs/
      - .current/
      - .playlists/
      - api/
        > Play a song
        =Itunes.songs 'Thunderstorm'
        |
      - docs/
        | Play specific song
        =itunes/songs/Around the Fur
        |
        | Play a playlist
        =itunes/playlist/class
      << itunes store/
      "

    @@use_pipe_delimiter = "set Applescript's text item delimiters to \"|\""

    def self.menu_after output, *args
      return if output   # MENU handled it, so don't interfere

      # /:song..., so play it

      if args[0] =~ /: (.+)/
        song = $1
        self.songs song
      end

    end

    def self.songs name=nil

      # /, so list all songs...

      if name.nil?
        tracks = Applescript.run "get the name of every track of library playlist 1 as string", :app=>"iTunes", :delimiter=>"|"

        tracks.sub! /^\"(.+)\"$/, "\\1"
        tracks = tracks.split("|")
        return tracks.sort.uniq.select{|o| o != "" && o !~ /^ /}.map{|o| ": #{o}\n"}.join
        return
      end

      # /song, so play it...

      name.sub!(/^: /, '')

      Applescript.run "iTunes", "play track \"#{name}\""
      "<*"
    end

    def self.current
      txt = Applescript.run "iTunes", "get name of current track"
      "- #{txt.gsub "\"", ''}/"
    end

    def self.playlists name=nil

      # /, so show playlists...

      if name.nil?
        tracks = Applescript.run "get name of every playlist as string", :app=>"iTunes", :delimiter=>"|"
        tracks.sub! /^\"(.+)\"$/, "\\1"
        tracks = tracks.split("|")
        return tracks.map{|o| ": #{o}\n"}.join
      end

      # /name, so play it...

      name.sub! /^: /, ''

      Applescript.run "iTunes", "play playlist \"#{name}\""
      "<*"
    end

    def self.play
      Applescript.run "iTunes", "play"
    end
    def self.pause
      Applescript.run "iTunes", "pause"
    end

    def self.next
      Applescript.run "iTunes", "next track"
      "<* #{self.current.gsub('\"', '')}"
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
      "<* Playing!"
    end
  end
end
