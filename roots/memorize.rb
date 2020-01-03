require 'uri'

class Memorize

  include Xiki   # To have View class, etc.

  attr :completed, :choices, :progress, :pending, :indent

  def self.menu *args

    options = yield
    task = options[:task]

    # /, so show example...

    return "
      > Expand this example to start memorizing
      | France : Paris
      | England : London
      | Japan : Tokyo
      | Germany : Berlin
      " if args == []

    raise "memorize command > didn't expect more than one item" if args.length != 1

    # /text\n, so start memorizing...
    if args[0] =~ /\n/ || args[0] =~ /\A>/

      # /~, so offer to save to memorize.com...

      return "
        * learn on Memorize.com
        * open progress
        * merge in
        " if task == []

      if task == ["learn on Memorize.com"]
        return self.browser args[0]
      elsif task == ["open progress"]
        View.open Memorize.filename
        return ""
      elsif task == ["merge in"]
        # View.open Memorize.filename
        # 1. Delete paragraph
        bounds = Tree.sibling_bounds
        new_lines = View.delete bounds[0], bounds[3]

        txt = File.read Memorize.filename

        # Iterate through each new line > and run this code
        new_lines.split("\n").each do |line|

          # Add number to index

          line1 = txt[/.+/]   # => "| [[2,4,1,4],[3]]"
          biggest = line1.scan(/\d+/).map{|o| o.to_i}.max + 1
          txt.sub!("],", ",#{biggest}],")   # => "| [[2,3,1,3,5],[4]]"
          line.sub!(/^  \|/, "| #{biggest}")
          txt << "#{line}\n"
        end
        File.open(Memorize.filename, "w") { |f| f << txt }

        # 2. Merge it into the file
          # | [[2,3,1,3],[4]]
          # Top line > get biggest number
          # Add it before > "],"
        return ""
      end

      # If task is "~memorize", it'll just continue on as normal expand, and start



      return self.start
    end

    # '+ show answer', so show it...
    if args[0] == "show answer"
      o = self.new(self.extract_view_txt)
      o.show_answer
      o.display
      return ""
    end

    # '+ I was wrong', so move to next step...
    if args[0] == "I was wrong"
      o = self.new(self.extract_view_txt)
      if task == []   # ^O, so do "right" instead of "wrong"
        o.i_was_right
      else   # ^X, so do "wrong"
        o.i_was_wrong
      end
      o.display
      return ""
    end

    # 'I was right', so move to next step...
    if args[0] == "I was right"
      o = self.new(self.extract_view_txt)
      o.i_was_right
      o.display
      return ""
    end
  end

  def self.filename
    name = View.file || View.name

    dir = File.expand_path "~/.xiki/misc/tmp/"

    FileUtils.mkdir_p dir
    "#{dir}/round1"+name.gsub(/[^a-z.]/i, '-')
  end

  def self.start

    # Clear out file state of any previous runs
    File.open(self.filename, "w") { |f| f << "" }

    # Extract text > contiguous |... lines...

    bounds = Tree.sibling_bounds(:must_match=>"[|+]")
    txt = View.txt bounds[0], bounds[3]

    return "
      - Error > every line must be in this format:
      | question : answer
    " if txt.split("\n").find{|o| o !~ / : /}

    View.delete bounds[0], bounds[3]
    o = self.new(txt)
    o.start

    View << "#{o.indent}- memorizing:\n"
    o.display

    ""

  end

  def start
    @choices = ["+ show answer"]

    @pending = @completed.reduce([{}, 1]) do |acc, o|
      acc[0][acc[1]] = o#[/ (.+)/, 1]
      acc[1] += 1
      acc
    end[0]

    # Shuffle (try up to 6 times if 1st one still at top)...

    length = @pending.length

    @progress = (1..length).to_a
    6.times{ @progress = @progress.sort_by{ rand } if @progress[0] == 1 }
    @progress = [@progress, []]

    @completed = ["#{@pending[@progress[0][0]].sub(/ : .*/, ' : ?')}"]

  end

  def self.extract_view_txt
    bounds = Tree.sibling_bounds(:must_match=>"[|+]")
    txt = View.delete bounds[0], bounds[3]
    txt
  end

  def serialize

    # Write hidden part to the file
    txt = serialize_hidden
    File.open(self.class.filename, "w") { |f| f << txt }

    # Return visible part
    return serialize_visible
  end

  def serialize_visible
    txt = ""
    txt = @completed.map{|o| "| #{o}"} + @choices
    txt = txt.map{|o| "#{o}\n"}.join("")

    # Prepend with "memorizing" if just starting

    txt
  end
  def serialize_hidden
    require 'json'

    txt = ""

    # pending = @pending.map{|k, v| "|*#{k} #{v}"}
    pending = @pending.map{|k, v| "| #{k} #{v}"}

    progress = @progress.any? ? ["| #{JSON[@progress]}"] : []

    txt = progress + pending
    txt = txt.map{|o| "#{o}\n"}.join("")
    txt
  end

  def deserialize txt
    txt_hidden = File.read self.class.filename
    txt << txt_hidden
    txt = txt.split "\n"

    # raise "- Every line must be of the format 'question : answer'"

    @completed, @choices, @progress, @pending = [], [], [], nil, nil

    @completed << txt.shift while txt.any? && txt[0] =~ / : /
    @choices << txt.shift while txt.any? && txt[0] =~ /^[+-]/

    @progress = txt.shift
    @progress = @progress ? JSON[@progress[/\[.+/]] : []

    @pending = txt

    @pending = @pending.reduce({}){|acc, o|
      match = o.match /(\d+) (.+)/
      acc[match[1].to_i] = match[2]
      acc
    }

  end

  def initialize txt=nil

    @indent = txt[/ +/]
    txt = txt.unindent
    txt = Tree.unquote txt

    return if ! txt
    deserialize txt
  end

  def inspect
    {
      :completed=>@completed,
      :choices=>@choices,
      :progress=>JSON[@progress],
      :pending=>@pending,
    }.ai
  end

  def propagate_edits

    active = @completed[-1]

    # If ends with ": ?", just replace with what's before it

    update_this = @pending[@progress[0][0]]

    if active =~ /(.+) : \?$/
      txt = $1
      update_this.sub! /.+? : /, "#{txt} : "
    else
      update_this.replace active #[/\| (.+)/, 1]
    end
  end

  def display
    txt = serialize
    txt.gsub! /^/, "#{@indent}"

    # No +... means the finished, so leave cursor at the top
    if txt !~ /^ *\+/
      View.<< txt, :dont_move=>1
      return Line.to_words
    end

    View << txt
    Move.up
    Move.up if Line =~ / *\+ I was right/
    Line.to_words
  end


  def show_answer
    propagate_edits

    @completed[-1] = "#{@pending[@progress[0][0]]}"
    @choices = ["+ I was wrong", "+ I was right"]

  end


  def i_was_wrong
    propagate_edits

    @completed.pop

    previous = @progress[0].shift
    @progress[0] << previous
    @progress[0].insert 2, previous if @progress[0].length > 1

    remove_runs

    @completed << "#{@pending[@progress[0][0]].sub(/ : .*/, ' : ?')}"
    @choices = ["+ show answer"]

    serialize
  end

  def remove_runs
    @progress[0] = @progress[0].reduce([[], ""]) do |acc, o|
      acc[0] << o if acc[1] != o
      acc[1] = o
      acc
    end[0]
  end

  def i_was_right
    propagate_edits

    @choices = ["+ show answer"]

    @completed.pop

    previous = @progress[0].shift

    # If that was the last one, show at top, and remember order
    if ! @progress[0].member? previous
      @completed << "#{@pending[previous]}"
      # Remember its order, and remove its redundant value from @pending
      @progress[1] << previous
      @pending.delete previous
    end

    # If that was the last one, they're finished!...

    if @progress[0].empty?

      finished
      @choices = []
      @progress = []
      txt = serialize
      # Clear out file state of any previous runs
      File.open(self.class.filename, "w") { |f| f << "" }
      return txt
    end

    # Show next question...

    @completed << "#{@pending[@progress[0][0]].sub(/ : .*/, ' : ?')}"

    serialize
  end

  def finished

    # memorizing on line before, so delete it
    if Line.value(0) =~ /^ *- memorizing:/
      View.delete(Line.left(0), Line.left)
    end

    result = []
    @completed.each_with_index{|item, i|
      result[@progress[1][i]-1] = item
    }
    @completed = result
  end

  def self.tasks_save_to_memorize

    # Grab paragraph...

    txt = View.paragraph

    # Open in browser...

    self.browser txt

  end

  # They right-clicked on a "a : b" table at the left margin

  def self.tasks_memorize

    # Grab paragraph...

    txt = View.paragraph

    txt = "memorize/\n#{txt.gsub(/^/, '  | ')}"
    View.open "memorize", :txt=>txt
    View.line = 2
    Launcher.launch

    # TODO > Cut off except final a:b lines (in case heading next to it)

    ""
  end

  def self.browser txt
    path = txt.gsub(" : ", ":").gsub("\n", "/").gsub(" ", "-")

    # Memorize.com always interprets commas as delimiter, even
    # when escaped, so just remove them for now
    path.gsub! ",", ""

    path = URI.encode path, "\","

    Browser.url "http://memorize.com/#{path}"
    "<* opened in browser"
  end

end
