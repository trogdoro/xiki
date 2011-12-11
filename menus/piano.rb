require 'rubygems'
gem 'midiator'
require 'midiator'

require "mode"

class Piano

  include MIDIator::Notes
  include MIDIator::Drums

  @@midi = nil
  @@program = 1
  @@names = ['Acoustic Grand Piano', 'Bright Acoustic Piano', 'Electric Grand Piano', 'Honky-tonk Piano', 'Electric Piano 1', 'Electric Piano 2', 'Harpsichord', 'Clavinet', 'Celesta', 'Glockenspiel', 'Music Box', 'Vibraphone', 'Marimba', 'Xylophone', 'Tubular Bells', 'Dulcimer', 'Drawbar Organ', 'Percussive Organ', 'Rock Organ', 'Church Organ', 'Reed Organ', 'Accordion', 'Harmonica', 'Tango Accordion', 'Acoustic Guitar (nylon)', 'Acoustic Guitar (steel)', 'Electric Guitar (jazz)', 'Electric Guitar (clean)', 'Electric Guitar (muted)', 'Overdriven Guitar', 'Distortion Guitar', 'Guitar harmonics', 'Acoustic Bass', 'Electric Bass (finger)', 'Electric Bass (pick)', 'Fretless Bass', 'Slap Bass 1', 'Slap Bass 2', 'Synth Bass 1', 'Synth Bass 2', 'Violin', 'Viola', 'Cello', 'Contrabass', 'Tremolo Strings', 'Pizzicato Strings', 'Orchestral Harp', 'Timpani', 'String Ensemble 1', 'String Ensemble 2', 'Synth Strings 1', 'Synth Strings 2', 'Choir Aahs', 'Voice Oohs', 'Synth Choir', 'Orchestra Hit', 'Trumpet', 'Trombone', 'Tuba', 'Muted Trumpet', 'French Horn', 'Brass Section', 'Synth Brass 1', 'Synth Brass 2', 'Soprano Sax', 'Alto Sax', 'Tenor Sax', 'Baritone Sax', 'Oboe', 'English Horn', 'Bassoon', 'Clarinet', 'Piccolo', 'Flute', 'Recorder', 'Pan Flute', 'Blown Bottle', 'Shakuhachi', 'Whistle', 'Ocarina', 'Lead 1 (square)', 'Lead 2 (sawtooth)', 'Lead 3 (calliope)', 'Lead 4 (chiff)', 'Lead 5 (charang)', 'Lead 6 (voice)', 'Lead 7 (fifths)', 'Lead 8 (bass + lead)', 'Pad 1 (new age)', 'Pad 2 (warm)', 'Pad 3 (polysynth)', 'Pad 4 (choir)', 'Pad 5 (bowed)', 'Pad 6 (metallic)', 'Pad 7 (halo)', 'Pad 8 (sweep)', 'FX 1 (rain)', 'FX 2 (soundtrack)', 'FX 3 (crystal)', 'FX 4 (atmosphere)', 'FX 5 (brightness)', 'FX 6 (goblins)', 'FX 7 (echoes)', 'FX 8 (sci-fi)', 'Sitar', 'Banjo', 'Shamisen', 'Koto', 'Kalimba', 'Bag pipe', 'Fiddle', 'Shanai', 'Tinkle Bell', 'Agogo', 'Steel Drums', 'Woodblock', 'Taiko Drum', 'Melodic Tom', 'Synth Drum', 'Reverse Cymbal', 'Guitar Fret Noise', 'Breath Noise', 'Seashore', 'Bird Tweet', 'Telephone Ring', 'Helicopter', 'Applause', 'Gunshot']
  @@map = {
    "A"=>57, "B"=>59, "C"=>60, "D"=>62, "E"=>64, "F"=>65, "G"=>67,
    "a"=>69, "b"=>71, "c"=>72, "d"=>74, "e"=>76, "f"=>77, "g"=>79,
    "h"=>81, "i"=>83, "j"=>84, "k"=>86, "l"=>88, "m"=>89, "n"=>91,
    "o"=>93, "p"=>95, "q"=>96, "r"=>98, "s"=>100, "t"=>101, "u"=>103,
    "v"=>105, "w"=>107, "x"=>108, "y"=>110, "z"=>112,
    "H"=>45, "I"=>47, "J"=>48, "K"=>50, "L"=>52, "M"=>53, "N"=>55,
    "O"=>33, "P"=>35, "Q"=>36, "R"=>38, "S"=>40, "T"=>41, "U"=>43,
    "V"=>21, "W"=>23, "X"=>24, "Y"=>26, "Z"=>28,

    '@'=>BassDrum2, '#'=>BassDrum1,
    '='=>SnareDrum2, '-'=>SnareDrum1,
    "'"=>ClosedHiHat, '"'=>OpenHiHat,
    '`'=>RideCymbal1, '^'=>CrashCymbal1, '*'=>CrashCymbal2, '<'=>Cowbell,
    '('=>MidTom1,  '|'=>MidTom2,  ')'=>LowTom2,
    }

  def self.menu
    "
    - .docs/
      | Single notes:
      - @piano/a/
      - @piano/b/
      - @piano/55/
      |
      | Multiple notes:
      - @piano/cde edc d  c   c de e
      - @piano/some words for fun
      |
      | Multiple parts:
      - .examples/
        - .chords/
        - .two parts/
        - .three parts/
        - .drums/
        - .unofficial xiki theme song/
    "
  end

  def self.chords
    "
    @piano/
      | A A A B A
      | C C D D C
      | E F F F E
    "
  end

  def self.two_parts
    "
    @piano/
      | CGcCGc C GaCGa  CGcCGc C GaCGa
      | cde edc d  c   c de e
    "
  end

  def self.three_parts
    "
    @piano/
      | ABCDEFGabcdefghijklmnopqrstuv
      |   B C D E F G a b c d e f g h
      |     B   C   D   E   F   G   a
    "
  end

  def self.drums
    "
    @piano/
      | ' ' ' ' ' ' * '
      | @  @=  @ | )=
    "
  end

  def self.unofficial_xiki_theme_song
    "
    @piano/
      |                 xiki is so great
      | W X W X W X W X W X W X W X W X
      | < < < < < < < < < < < < < < < <
      | @ = @@= @ = @@= @ = @@= @ = @@=
    "
  end

  def self.menu_before *args

    # If nothing or docs/, don't interject
    return nil if args == [] || args[0] == "docs"

    # If just number, intercept
    if args.length == 1 && args[0] =~ /^\d+$/
      self.note args[0].to_i
      return false
    end

    # Assume it's a string of music

    txt = ENV['txt']
    lines = txt.split("\n")
    lines = lines.map{|o| o.split(//)}

    # TODO: Instead, start at where cursor is
    # if at end, go to beginning
    # if column is less than beginning, move cursor to beginning
    # else, do this:
    #   just grab column and subtract off length of /^.*(\/|\| )/

    before_notes = Line.value[/.+(\/|\| ?)/]
    View.column = before_notes.length

    (0..lines[0].length-1).each do |i|
      lines.each do |line|
        self.note line[i], :no_sit=>1
      end
      Move.forward unless View.cursor == Line.right
      $el.sit_for 0.15
      Piano.clear
      Piano.clear 10
    end

    false
  end

  def self.clear channel=1
    @@midi.driver.control_change 123, channel, 123
  end

  def self.<< letter
    self.note letter
  end

  def self.note letter='a', options={}
    return if letter.nil?

    channel, volume = 1, 126
    if letter.is_a? Fixnum
      number = letter
    elsif letter =~ /^[a-zA-Z]$/
      number = @@map[letter]
    elsif letter.length == 1 && letter.count("@#='\"`^*<(|)-") == 1
      channel = 10
      number = @@map[letter]
      volume = 65 unless letter.count("@#=-") == 1
    elsif letter == " "
      number = 0
    elsif letter.is_a?(String) && letter.length > 1
      letter.split('').each{|o| self.note o}
      return
    else
      raise "- Note #{letter.inspect} not recognized!"
    end

    @@midi.driver.note_on(number, channel, volume) unless number == 0

    return if options[:no_sit]

    $el.sit_for 0.15
    self.clear channel
    nil
  end

  def self.key letter, note, channel=1, velocity=126
    $el.define_key(:piano_mode_map, letter) do
      @@midi.driver.note_on(note, channel, velocity)
    end
  end

  def self.midi
    @@midi
  end

  def self.keys
    $el.elvar.piano_mode_map = $el.make_sparse_keymap unless $el.boundp :piano_mode_map

    key " ", BassDrum2, 10
    key "-", SnareDrum2, 10
    key "1", SnareDrum1, 10

    key "`", ClosedHiHat, 10, 60
    key "1", OpenHiHat, 10, 60
    key $el.kbd("C-i"), RideCymbal1, 10, 60

    # Note: keys are optimized for dvorak
    key ";", C3; key "o", Cs3
    key "q", D3; key "e", Ds3
    key "j", E3

    key "k", F3; key "i", Fs3
    key "x", G3; key "d", Gs3
    key "b", A3; key "h", As3
    key "m", B3
    key "w", C4; key "n", Cs4
    key "v", D4; key "s", Ds4
    key "z", E4

    key "'", B3; key "3", Cs4
    key ",", C4; key "3", Cs4
    key ".", D4; key "4", Ds4
    key "p", E4
    key "y", F4; key "6", Fs4
    key "f", G4; key "7", Gs4
    key "g", A4; key "8", As4
    key "c", B4
    key "r", C5; key "0", Cs5
    key "l", D5; key "[", Ds5
    key "/", E5
    key "=", F5
    key '\\\\', G5

    "aut259]".split(//).each do |letter|
      $el.define_key(:piano_mode_map, letter) do
        View.message("key '#{letter}' inactive")
      end

    end

    key "", :nothing   # nothing


    $el.define_key(:piano_mode_map, $el.kbd("<right>")) do
      @@program += 1
      View.message "program: #{@@program+1} - #{@@names[@@program]}"
      @@midi.program_change 1, @@program
    end

    $el.define_key(:piano_mode_map, $el.kbd("<left>")) do
      @@program -= 1
      View.message "program: #{@@program+1} - #{@@names[@@program]}"
      @@midi.program_change 1, @@program
    end

    $el.define_key(:piano_mode_map, $el.kbd("<backspace>")) do
      @@midi.control_change 123, 1, 123
    end
  end

  def self.init
    self.keys
    # Make piano mode happen for .piano files
    Mode.define(:piano, ".piano") do
      Notes.apply_styles
    end

    if @@midi.nil?
      @@midi = MIDIator::Interface.new
      @@midi.use :dls_synth
      @@midi.control_change 32, 10, 1 # TR-808 is Program 26 in LSB bank 1
      @@midi.program_change 10, 26
    end
  end
end

Piano.init   # Define mode
Menu.drums :menu=>'piano'
