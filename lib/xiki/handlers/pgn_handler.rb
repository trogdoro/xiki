module Xiki
  class PgnHandler
    def self.handle options

      options[:no_search] = 1

      source = options[:handlers]['pgn']
      return if ! source || options[:output] || options[:halt]

      args = options[:args]

      # If /foo/, start with step 1...

      return options[:output] = "- 1/" if ! args

      step, command = args

      # /foo/N, so show step...

      if ! command

        file = "#{options[:enclosing_source_dir]}#{source}"

        nth = step[/\d+/].to_i*2 + (step =~ /\./ ? 0 : -1)
        txt = self.load_pgn file, nth
        txt = self.fen_to_pgn txt
        txt = "- next/\n- previous/\n#{txt}"
        options[:output] = txt
        #       options[:output] = Xiki::Chess::NEW_BOARD
        return
      end

      # /foo/N/next, so go to next (or previous)...

      Tree.to_parent
      Tree.collapse

      launch_options = {}
      step_next = if command == "previous"
          launch_options[:line_found] = 2
          step =~ /\.$/ ? step[/\d+/] : "#{step[/\d+/].to_i - 1}.."
        else   # - next/
          step =~ /\.$/ ? (step[/\d+/].to_i + 1) : "#{step}.."
        end

      step_next = "1" if step_next == "0.."

      Line.sub!(/\d+\.*/, step_next.to_s)
      Launcher.launch launch_options

    end

    def self.load_pgn file, step
      gem "chess"
      require "chess"
      pgn = ::Chess::Pgn.new(file)
      game = ::Chess::Game.new
      moves = pgn.moves
      at_end = step >= moves.length

      moves[0..step-1].each { |m| game.move(m) }
      txt = game.board.to_s
      txt = self.from_fen txt
      txt = "#{txt}x\n" if at_end
      txt
    end

    def self.from_fen txt
      txt.gsub!(/.\[\d+m/, '')
      txt.gsub!(/^ .+/, '')
      txt.gsub!(/^\d /, '')
      txt.gsub!(/ +$/, '')
    end

    def self.black_squares txt, odd=false
      txt.gsub!(/(.)(.)/) do   # Upcase every other one (or change to +)
        a, b, = $1, $2
        if odd
          a = a == " " ? "+" : a.upcase
        else
          b = b == " " ? "+" : b.upcase
        end
        "#{a}#{b}"
      end
      txt
    end

    def self.fen_to_pgn txt
      txt.gsub!(/ /, '')
      txt.tr! "rnbqkp", "tmvwlo"
      txt.tr! "RNBQKP", "rnbqkp"
      txt.tr! ".", " "

      txt.gsub!(/(.+\n)(.+\n)/) do   # Upcase every other one (or change to +)
        "#{self.black_squares $1}#{self.black_squares $2, 1}"
      end

      txt.gsub! /^/, "|:"
      txt
    end

  end
end
