Xiki::Requirer.require_gem 'unimidi', :optional=>1

require 'xiki/core/mode'


module Xiki
  class Piano

    include MIDIator::Notes rescue nil
    include MIDIator::Drums rescue nil

    @@unimidi = nil

    @@held_down = []

    @@midi = nil
    @@velocity = 100
    @@tempo = 120
    @@probability = 100
    @@melodic = 0
    @@melodic_accumulator = []
    @@climb = 0
    @@pentatonic = false
    @@variation = 0
    @@consistency = 0
    @@mode = 0
    @@octave = 0
    @@program = 1
    @@repeat = 1
    @@names = ['Acoustic Grand Piano', 'Bright Acoustic Piano', 'Electric Grand Piano', 'Honky-tonk Piano', 'Electric Piano 1', 'Electric Piano 2', 'Harpsichord', 'Clavinet', 'Celesta', 'Glockenspiel', 'Music Box', 'Vibraphone', 'Marimba', 'Xylophone', 'Tubular Bells', 'Dulcimer', 'Drawbar Organ', 'Percussive Organ', 'Rock Organ', 'Church Organ', 'Reed Organ', 'Accordion', 'Harmonica', 'Tango Accordion', 'Acoustic Guitar (nylon)', 'Acoustic Guitar (steel)', 'Electric Guitar (jazz)', 'Electric Guitar (clean)', 'Electric Guitar (muted)', 'Overdriven Guitar', 'Distortion Guitar', 'Guitar harmonics', 'Acoustic Bass', 'Electric Bass (finger)', 'Electric Bass (pick)', 'Fretless Bass', 'Slap Bass 1', 'Slap Bass 2', 'Synth Bass 1', 'Synth Bass 2', 'Violin', 'Viola', 'Cello', 'Contrabass', 'Tremolo Strings', 'Pizzicato Strings', 'Orchestral Harp', 'Timpani', 'String Ensemble 1', 'String Ensemble 2', 'Synth Strings 1', 'Synth Strings 2', 'Choir Aahs', 'Voice Oohs', 'Synth Choir', 'Orchestra Hit', 'Trumpet', 'Trombone', 'Tuba', 'Muted Trumpet', 'French Horn', 'Brass Section', 'Synth Brass 1', 'Synth Brass 2', 'Soprano Sax', 'Alto Sax', 'Tenor Sax', 'Baritone Sax', 'Oboe', 'English Horn', 'Bassoon', 'Clarinet', 'Piccolo', 'Flute', 'Recorder', 'Pan Flute', 'Blown Bottle', 'Shakuhachi', 'Whistle', 'Ocarina', 'Lead 1 (square)', 'Lead 2 (sawtooth)', 'Lead 3 (calliope)', 'Lead 4 (chiff)', 'Lead 5 (charang)', 'Lead 6 (voice)', 'Lead 7 (fifths)', 'Lead 8 (bass + lead)', 'Pad 1 (new age)', 'Pad 2 (warm)', 'Pad 3 (polysynth)', 'Pad 4 (choir)', 'Pad 5 (bowed)', 'Pad 6 (metallic)', 'Pad 7 (halo)', 'Pad 8 (sweep)', 'FX 1 (rain)', 'FX 2 (soundtrack)', 'FX 3 (crystal)', 'FX 4 (atmosphere)', 'FX 5 (brightness)', 'FX 6 (goblins)', 'FX 7 (echoes)', 'FX 8 (sci-fi)', 'Sitar', 'Banjo', 'Shamisen', 'Koto', 'Kalimba', 'Bag pipe', 'Fiddle', 'Shanai', 'Tinkle Bell', 'Agogo', 'Steel Drums', 'Woodblock', 'Taiko Drum', 'Melodic Tom', 'Synth Drum', 'Reverse Cymbal', 'Guitar Fret Noise', 'Breath Noise', 'Seashore', 'Bird Tweet', 'Telephone Ring', 'Helicopter', 'Applause', 'Gunshot']

    begin
      @@map = {
        '@'=>BassDrum2, '#'=>BassDrum1,
        '='=>SnareDrum2, '-'=>SnareDrum1,
        "'"=>ClosedHiHat, '"'=>OpenHiHat,
        '^'=>RideCymbal1, '`'=>CrashCymbal1, '*'=>CrashCymbal2, '<'=>Cowbell,
        '['=>MidTom1,  '_'=>MidTom2,  ']'=>LowTom2,
        }
    rescue Exception=>e
    end

    MENU = %`
      > Pass in notes (start GarageBand first)
      | g cdefg c c
      - .setup/
        - .instrument/
          - most common/
            - Acoustic Grand Piano/
            - Electric Piano 1/
            - Glockenspiel/
            - Vibraphone/
            - Xylophone/
            - Drawbar Organ/
            - Church Organ/
            - Accordion/
            - Acoustic Guitar (nylon)/
            - Distortion Guitar/
            - Electric Bass (finger)/
            - Violin/
            - Tremolo Strings/
            - Pizzicato Strings/
            - Orchestral Harp/
            - Timpani/
            - String Ensemble 2/
            - Synth Strings 2/
            - Choir Aahs/
            - Synth Choir/
            - Orchestra Hit/
            - Trumpet/
            - Flute/
            - Pan Flute/
            - Lead 1 (square)/
            - Lead 2 (sawtooth)/
            - Pad 1 (new age)/
            - Pad 2 (warm)/
            - Pad 4 (choir)/
            - Pad 7 (halo)/
            - Pad 8 (sweep)/
            - FX 3 (crystal)/
            - FX 6 (goblins)/
            - Steel Drums/
            - Woodblock/
            - Taiko Drum/
          - all/
        - .tempo/
          - 60
          - 120
          - 240
          - 480
          - 960
        - .repeat/
          - 1
          - 2
          - 4
          - 8
        - .mode/
          - lydian) 4
          - ionian - major) 3
          - mixolydian) 2
          - dorian) 1
          - aeolian - minor) 0
          - phrygian) -1
          - locrian) -2
          - random/
        - .pentatonic/
          - off
          - on
        - .octave/
          - 2
          - 1
          - 0
          - -1
          - -2
        - .velocity/
          - 126
          - 96
          - 64
          - 32
        - .variation/
          - 0
          - 1
          - 2
          - 3
        - .consistency/
          - 0%
          - 25%
          - 50%
          - 75%
        - .melodic/
          - 1
          - 0
        - .climb/
          - 1
          - -1
          - 0
        - .probability/
          - 100%
          - 75%
          - 50%
        - .reset/
      - .random notes/
      - examples/
        - basics/
          - chords/
            @piano/
              | A A A B A
              | C C D D C
              | E F F F E
          - two parts/
            @piano/
              | CGcCGc C GaCGa  CGcCGc C GaCGa
              | cde edc d  c   c de e
          - three parts/
            @piano/
              | ABCDEFGabcdefghijklmnopqrstuv
              |   B C D E F G a b c d e f g h
              |     B   C   D   E   F   G   a
          - sharps/
            @piano/
              |             #    # # # #
              | ce ecbca    Gb baGbG G G a
              |                    #
              |  C E C A C L B E B N B L H
          - drums/
            @piano/
              | ' ' ' ' ' '*  '
              | =@@= @@@=@@= @@@
          - unofficial xiki theme song/
            @piano/
              |                 xiki is so great
              | P Q P Q P Q P Q P Q P Q P Q P Q
              | < < < < < < < < < < < < < < < <
              | @ = @@= @ = @@= @ = @@= @ = @@=
        - generation/
          - variation/
            @piano/
              | reset()
              | variation()
              | aaaaaaaa
          - probability/
            @piano/
              | reset()
              | variation()
              | probability(50)
              | aaaaaaaaaaaa
          - melodic/
            @piano/
              | reset()
              | variation(1)
              | melodic()
              | aaaaaaaaaaaa
          - climb/
            @piano/
              | reset()
              | variation(1)
              | melodic()
              | climb()
              | aaaaaaaaaaaa
          - consistency/
            @piano/
              | reset()
              | variation()
              | consistency(80)
              | aaaaaaaaaaaa
          - solo/
            @piano/
              | reset()
              | repeat(8)
              | tempo(55)
              | mode(rand 100)
              | variation()
              | consistency(50)
              | melodic()
              | probability(80)
              | abcdefgh
          - duet/
            @piano/
              | reset()
              | repeat(8)
              | tempo(35)
              | mode(rand 8)
              | variation(2)
              | consistency(10)
              | melodic()
              | A A A A
              |  h h h h
          - all together/
            @piano/
              | reset()
              | repeat(8)
              | tempo(45)
              | mode(rand 100)
              | variation()
              | consistency(50)
              | melodic()
              | probability(80)
              | aaaaaaaa
              | AAAAAAAA
              | H   H
          - everything random/
            @piano/
              | reset()
              | repeat(16)
              | tempo(rand(60) + 40)
              | mode(rand 200)
              | variation(rand(6) + 1)
              | consistency(rand(50))
              | melodic(rand 1)
              | octave(-1)
              | probability(rand(55) + 45)
              | aaaaaaaaaaaaaaaa
              | AAAAAAAAAAAAAAAA
              | H   H   H   H
          - cool instruments/
            @piano/
              | reset()
              | repeat(32)
              | tempo(rand(60) + 40)
              | mode(rand 200)
              | variation(rand(6) + 1)
              | instrument([112, 108, 107, 102, 101, 100, 98, 97, 96, 95, 89, 88, 87, 86, 80, 54, 50, 49, 46, 45, 34, 12, 11, 10, 9, 8, 4][rand 27])
              | consistency(rand(50))
              | melodic(rand 1)
              | octave(-1)
              | probability(rand(55) + 45)
              | aaaaaaaaaaaaaaaa
              | AAAAAAAAAAAAAAAA
              | H   H   H   H
      - .api/
        | Play some notes
        @ Piano.song "abc"
      - .docs/
        > Single notes
        - @piano/a/
        - @piano/55/

        > Multiple notes
        - @piano/cde edc d  c   c de e
        - @piano/some words for fun

        > Modes
        modes/
          | For reference, here are the whole and half steps for the options under
          | the "mode" menu.  By default the A scale is used.
          |
          | - O o o oo o oO: lydian
          | - O o oo o o oO: ionian (major)
          | - O o oo o oo O: mixolydian
          | - O oo o o oo O: dorian
          | - O oo o oo o O: aeolian (minor)
          | - Oo o o oo o O: phrygian
          | - Oo o oo o o O: locrian
      `

    def self.names
      @@names
    end


    def self.menu_after menu_output, *args

      # Don't interfere if menu did something
      return menu_output if menu_output

      # If just number, intercept
      if args.length == 1 && args[0] =~ /^\d+$/
        self.note args[0].to_i
        return false
      end

      if $el
        if Line =~ /\(/
          Tree.to_parent
          Move.to_end
          Search.forward("^[^(\n]+$")
        end
        txt = args[0]
        return "@beg/quoted/" if txt !~ /\n/
      else
        txt = args[0]
      end

      self.song txt, :move=>1

      nil
    end

    def self.song txt, options={}
      @@lines = txt.split("\n")#.reverse

      self.extract_functions

      # If only config, just run first ones
      #     if @@lines.empty?
      #       return self.run_functions @@functions_by_index[0], :include_all
      #     end
      self.run_functions @@functions_by_index[0], :include_all

      repeat = (@@functions_by_index[0]||[]).find{|o| o =~ /^rep(eat)?\(/}
      @@repeat = (repeat[/\d+/]||"4").to_i if repeat

      @@repeat.times do |i|

        # Start at where cursor is
        if $el
          # Comment out for demo?
          View.column = Line.value[/.+(\/|\| ?)/].length if options[:move]
        end

        longest = @@lines.inject(0){|acc, e| e.length > acc ? e.length : acc}

        longest.times do |j|
          #         self.run_functions @@functions_by_index[j] # unless j == 0
          self.run_functions @@functions_by_index[j] unless i == 0

          sharp = false
          @@lines.each_with_index do |line, track|
            char = line[j] ? line[j].chr : nil
            self.note char, :no_sit=>1, :sharp=>sharp, :track=>track
            sharp = char == "#"
          end
          if $el
            # Comment out for demo?
            Move.forward if options[:move] && View.cursor != Line.right
          end
          self.pause
        end
      end

      self.clear

      nil
    end

    def self.run_functions list, include_all=nil
      @@in_run_functions = true
      (list||[]).each do |item|
        next if !include_all && item =~ /^rep(eat)?\(/
        eval("Piano.#{item}")
      end
      @@in_run_functions = false
    end

    def self.extract_functions
      lines, @@lines = @@lines.partition{|i| i =~ /\(/}

      @@functions_by_index = {}
      lines.each do |line|
        Code.parse_functions line, @@functions_by_index
      end
    end

    def self.<< letter
      self.note letter
    end

    def self.letter_to_number letter, options={}

      adjustment = @@mode

      letter.next! if @@pentatonic && (letter == "b" || letter == "e")

      number = letter[0,1].sum

      number = case letter
      when "a".."z";  number - 96
      when "A".."G";  number - 71
      when "H".."N";  number - 85
      when "O".."U";  number - 99
      when "V".."Z";  number - 111
      else; raise "Don't know how to convert the note #{letter} to a number."
      end

      number = self.apply_variation number, options

      number = number * 12/7.0
      number -= 0.01
      adjustment -= 2
      adjustment = (adjustment / 7.0) - 0.01
      number += adjustment

      number = number.floor
      number += 68
      number
    end

    def self.apply_variation number, options={}
      return number if rand(100) > (100 - @@consistency)   # Do nothing if consistency says to stop

      random = rand(@@variation+1)

      if @@climb == 0
        random *= ((-1) ** rand(2))   # Half of the time, make it decrease note
      end

      if @@melodic == 1
        track = options[:track]
        @@melodic_accumulator[track] ||= 0
        random *= @@climb if @@climb != 0
        @@melodic_accumulator[track] += random

        @@melodic_accumulator[track] = 0 if @@melodic_accumulator[track] > 28 || @@melodic_accumulator[track] < -28

        random = @@melodic_accumulator[track]
      end

      number + random
    end

    def self.apply_probability number
      return 0 if rand(100) > @@probability
      number
    end

    def self.note letter='a', options={}

      return if letter.nil? || letter == "#"

      channel = 1
      velocity = @@velocity

      # Varying the velocity doesn't sound super-great
      #     velocity = 63 + rand(63)

      if letter.is_a? Fixnum
        number = letter
        # If super-low, make them audible
        number += 69 if number <= 20
      elsif letter =~ /^[0-9]$/
        number = letter[0] + 21
      elsif letter =~ /^[a-zA-Z]$/
        number = self.letter_to_number letter, options
        number = self.apply_probability number
      elsif letter.length == 1 && letter.count("@#='\"`^*<[_]-") == 1
        channel = 10
        number = @@map[letter]
        velocity = letter.count("@#=-") == 1 ? 126 : 65
      elsif letter == " " || letter == "+"
        number = 0
      elsif letter.is_a?(String) && letter.length > 1
        letter.split('').each{|o| self.note o}
        return
      else
        raise "- Note #{letter.inspect} not recognized!"
      end
      return if number == 0

      number += 1 if options[:sharp]
      number += (@@octave * 12)

      channel = 143 + channel
      @@held_down << number
      self.unimidi.puts(channel, number, velocity) # note on message

      return if options[:no_sit]   # Don't sit if other tracks have same beat

      self.pause
      nil
    end

    def self.unimidi
      @@unimidi ||= UniMIDI::Output.open(0).open
    end

    def self.send channel, number, value

      output = UniMIDI::Output.open(0)

      output.open do |output|
        output.puts(channel, number, 90) # note on message
      end
    end

    def self.pause
      pause = @@tempo * 4
      pause = pause / 60.0
      pause = 1 / pause
      $el ? $el.sit_for(pause) : sleep(pause)
      Piano.clear
    end

    def self.clear chan=1
      chan = (chan + 143) - 16

      while number = @@held_down.shift do
        self.unimidi.puts(chan, number, 90) # note on message
      end
    end

    def self.keydef letter, note, channel=1, velocity=126
      $el.define_key(:piano_mode_map, letter) do
        self.driver.note_on(note, channel, velocity)
      end
    end

    def self.midi
      @@midi || self.connect
    end

    def self.keys
      $el.elvar.piano_mode_map = $el.make_sparse_keymap unless $el.boundp :piano_mode_map

      keydef " ", BassDrum2, 10
      keydef "-", SnareDrum2, 10
      keydef "1", SnareDrum1, 10

      keydef "6", ClosedHiHat, 10, 60
      keydef "1", OpenHiHat, 10, 60
      keydef $el.kbd("C-i"), RideCymbal1, 10, 60

      # Note: keys are optimized for dvorak
      keydef ";", C3; keydef "o", Cs3
      keydef "q", D3; keydef "e", Ds3
      keydef "j", E3

      keydef "k", F3; keydef "i", Fs3
      keydef "x", G3; keydef "d", Gs3
      keydef "b", A3; keydef "h", As3
      keydef "m", B3
      keydef "w", C4; keydef "n", Cs4
      keydef "v", D4; keydef "s", Ds4
      keydef "z", E4

      keydef "'", B3; keydef "3", Cs4
      keydef ",", C4; keydef "3", Cs4
      keydef ".", D4; keydef "4", Ds4
      keydef "p", E4
      keydef "y", F4; keydef "6", Fs4
      keydef "f", G4; keydef "7", Gs4
      keydef "g", A4; keydef "8", As4
      keydef "c", B4
      keydef "r", C5; keydef "0", Cs5
      keydef "l", D5; keydef "[", Ds5
      keydef "/", E5
      keydef "=", F5
      keydef '\\\\', G5

      "aut259]".split(//).each do |letter|
        $el.define_key(:piano_mode_map, letter) do
          View.message("key '#{letter}' inactive")
        end

      end

      keydef "", :nothing   # nothing


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

    rescue Exception=>e

    end

    def self.init
      self.keys
      # Make piano mode happen for .piano files
      Mode.define(:piano, ".piano") do
        Notes.apply_styles
      end
    end

    def self.instrument type, name=nil, options={}
      # If 'all' show all
      return @@names.map{|o| "- #{o}/"}.join("\n") if type == 'all' && name.nil?

      # If 1 arg, assume it's the name
      name = type if name.nil?


      if name.is_a?(Fixnum)
        index = name
      else
        name = Regexp.escape name
        name.gsub! '\ ', ".+"
        index = @@names.index{|o| o =~ /^#{name}$/i}
        index ||= @@names.index{|o| o =~ /^#{name}/i}
        index ||= @@names.index{|o| o =~ /#{name}/i}
      end

      @@midi.program_change 1, (index||0)
      return if options[:quiet]

      # TODO: turn back on, but not when called from .run_functions
        # How to detect?
          # or, maybe just set instance variable when in .run_functions
            # @@in_run_functions = true
          # maybe .sub! it to have an extra parameter to suppress?
            # instrument('Piano') -> instrument('Piano', :suppress)
      Piano.note "abc" unless @@in_run_functions
    end

    def self.reset

      @@velocity = 126
      @@tempo = 120
      @@probability = 100

      @@variation = 0
      @@melodic = 0
      @@melodic_accumulator = []
      @@climb = 0
      @@pentatonic = false
      @@consistency = 0
      @@mode = 0
      @@octave = 0
      @@program = 1
      @@repeat = 1
      # @@seed = nil

      "@flash/- success!"
    end

    #
    # Low level call to configure midi.
    #
    def self.control_change a, b, c
      self.driver.control_change a, b, c
    end

    def self.connect
      @@midi = MIDIator::Interface.new
      @@midi.use :dls_synth
      # This doesn't work in Lion :(
      @@midi.control_change 32, 10, 1   # Drums: R-808 is Program 26 in LSB bank 1
      @@midi.control_change 7, 1, 126   # Turn volume up to max
      @@midi
    end

    def self.velocity txt="126";  @@velocity = txt.to_i;  "@flash/- updated!";  end
    def self.tempo txt="120";  @@tempo = txt.to_i;  "@flash/- updated!";  end
    def self.probability txt="50";  @@probability = txt.to_s.sub('%', '').to_i;  "@flash/- updated!";  end
    def self.variation txt="2"
      @@variation = txt.to_i
      # if @@seed   # If seed set manually, just use it
      #   seed = @@seed
      # else   # Else auto-generate seed
      #   seed = rand 999_999_999_999_999_999_999
      # end

      # srand seed

      "@flash/- updated!"
    end
    def self.melodic txt="1";  @@melodic = txt.to_i;  "@flash/- updated!";  end
    def self.climb txt="1";  @@climb = txt.to_i;  "@flash/- updated!";  end
    def self.pentatonic txt="1";  Ol.<<(txt); @@pentatonic = [true, "on", 1].member?(txt);  "@flash/- updated!";  end
    def self.consistency txt="50";  @@consistency = txt.to_s.sub('%', '').to_i;  "@flash/- updated!";  end
    def self.octave txt="0";  @@octave = txt.to_i;  "@flash/- updated!";  end
    def self.repeat txt="4"; @@repeat = txt.to_i;  "@flash/- updated!";  end
    # def self.seed txt; @@seed = txt.to_i;  "@flash/- updated!";  end

    def self.mode txt=nil
      return @@mode if txt.nil?

      if txt.to_s == "random"
        random = (-2..9).to_a[rand 7]
        @@mode = random
        return "@flash/- updated to #{random}!"
      end

      @@mode = txt.to_i
      "@flash/- updated!"
    end

    class << self
      alias :tem :tempo
      alias :pro :probability
      alias :mel :melodic
      alias :cli :climb
      alias :vel :velocity
      alias :pen :pentatonic
      alias :res :reset
      alias :rep :repeat
      alias :var :variation
      alias :con :consistency
      alias :oct :octave
      alias :mod :mode
      alias :ins :instrument
    end

    def self.driver
      @@midi ||= self.connect
    end

    def self.random_notes range=nil, notes=nil
      if range.nil?
        return "- a..g/"
      end

      # If just range, generate

      if notes.nil?

        range ||= "a..g"

        range =~ /(.+)\.\.(.+)/
        range = $1..$2

        range = range.to_a

        txt = ""
        8.times do
          txt << range[rand range.length]
        end
        return "| #{txt}"
      end

      self.song ENV['txt']
      nil
    end

  end

  Piano.init   # Define mode
  Menu.drums :menu=>'piano'
end
