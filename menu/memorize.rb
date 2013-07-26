class Memorize

  MENU = "
    > Example
    | France : Paris
    | England : London
    | Japan : Tokyo
    | Germany : Berlin
    - docs/
      | Add some facts like the example, and launch one of the
      | example lines to start an interactive memorize process.
      | In addition to helping you memorize, this is a low-stress
      | way to digest and review facts.
      | Double-click on one of to lines to begin.
    "

  MENU_OBSCURED = %`
    - .show answer/
    - .I was wrong/
    - .I was right/
    - .test/
      - deserialize/
        : > Example
        : | Germany : Berlin
        : | China : Beijing
        : | Japana : ___
        : - show answer/
        : |#[[3,1,2],[]]
        : |#1 France : Paris
        : |#2 England : London
        : |#3 Japan : Tokyo
      - serialize/
        : {
        :   :heading   => [
        :     "> Example"
        :   ],
        :   :completed => [
        :     "| Germany : Berlin",
        :     "| China : Beijing"
        :   ],
        :   :choices   => [
        :     "- show answer/"
        :   ],
        :   :progress  => [[3,1,2],[]],
        :   :pending   => {
        :     1 => "France : Paris",
        :     2 => "England : London",
        :     3 => "Japan : Tokyo"
        :   }
        : }
    `

  attr :heading, :completed, :choices, :progress, :pending

  def self.test kind, txt
    {
      'deserialize'=> -> {
        o = self.new
        o.deserialize(txt)
        Tree.quote o.inspect
      },
      'serialize'=> -> {
        o = self.new
        hash = eval txt
        hash.each{ |key, val| o.instance_variable_set "@#{key}", val }
        Tree.quote o.serialize
      },
    }[kind][]
  end

  def serialize
    txt = ""

    pending = @pending.map{|k, v| "|##{k} #{v}"}

    progress = @progress.any? ? ["|##{JSON[@progress]}"] : []

    txt = @heading + @completed + @choices + progress + pending
    txt = txt.map{|o| "#{o}\n"}.join("")
    txt
  end

  def deserialize txt
    txt = txt.split "\n"

    @heading, @completed, @choices, @progress, @pending = [], [], [], nil, nil

    @heading << txt.shift while txt.any? && txt[0] !~ /\|/
    @completed << txt.shift while txt.any? && txt[0] =~ /\|/
    @choices << txt.shift while txt.any? && txt[0] !~ /\|/

    @progress = txt.shift
    @progress = @progress ? JSON[@progress[/\[.+/]] : []

    @pending = txt


    @pending = @pending.reduce({}){|acc, o|
      match = o.match /(\d+) (.+)/
      acc[match[1].to_i] = match[2]
      acc
    }

  end

  def inspect
    {
      :heading=>@heading,
      :completed=>@completed,
      :choices=>@choices,
      :progress=>JSON[@progress],
      :pending=>@pending,
    }.ai
  end

  def initialize txt=nil
    deserialize txt if txt
  end

  def set_line_found options
    options[:no_search] = 1
    return if @progress[0].empty?
    options[:line_found] = @heading.length + @completed.length + 1
  end

  def self.show_answer txt=nil
    return "@beg/neighbors/" if ! txt   # Beg for consecutive lines if not passed yet

    o = self.new txt
    txt = o.show_answer
    o.set_line_found yield

    "@instead/neighbors/\n#{txt.gsub /^/, '  '}"
  end

  def show_answer
    propagate_edits

    @completed[-1] = "| #{@pending[@progress[0][0]]}"
    @choices = ["- I was wrong/", "- I was right/"]

    serialize
  end


  def self.i_was_wrong txt=nil
    return "@beg/neighbors/" if ! txt   # Beg for consecutive lines if not passed yet

    o = self.new txt
    txt = o.i_was_wrong
    o.set_line_found yield

    "@instead/neighbors/\n#{txt.gsub /^/, '  '}"
  end

  def i_was_wrong
    propagate_edits

    @completed.pop

    previous = @progress[0].shift
    @progress[0] << previous
    @progress[0].insert 2, previous if @progress[0].length > 1

    remove_runs

    @completed << "| #{@pending[@progress[0][0]].sub(/ : .+/, ' : ___')}"
    @choices = ["- show answer/"]

    serialize
  end

  def remove_runs
    @progress[0] = @progress[0].reduce([[], ""]) do |acc, o|
      acc[0] << o if acc[1] != o
      acc[1] = o
      acc
    end[0]
  end


  def self.i_was_right txt=nil
    return "@beg/neighbors/" if ! txt   # Beg for consecutive lines if not passed yet

    o = self.new txt
    txt = o.i_was_right
    o.set_line_found yield

    "@instead/neighbors/\n#{txt.gsub /^/, '  '}"
  end

  def i_was_right
    propagate_edits

    @choices = ["- show answer/"]

    @completed.pop

    previous = @progress[0].shift

    # If that was the last one, show at top, and remember order
    if ! @progress[0].member? previous
      @completed << "| #{@pending[previous]}"
      # Remember its order, and remove its redundant value from @pending
      @progress[1] << previous
      @pending.delete previous
    end

    # If that was the last one, they're finished!...

    if @progress[0].empty?
      readjust_order
      @choices = []
      @progress = []
      return serialize
    end

    # Show next question...

    @completed << "| #{@pending[@progress[0][0]].sub(/ : .+/, ' : ___')}"

    serialize
  end


  def readjust_order
    result = []
    @progress[1].each{|i| result << @completed[i-1]}
    @completed = result
    "hi"
  end


  def self.menu_after output, *path

    # We only want to interject if they're starting out,
    # which means launching a quoted line that got turned into linebreaks
    return if path.length != 1

    return if output   # Always return if there was output, because it was handled

    # |#, so beg for all lines
    return "@beg/neighbors/" if path[0] =~ /\A[|>].+\z/

    txt = path[0]

    o = self.new txt
    txt = o.start
    o.set_line_found yield

    "@instead/neighbors/\n#{txt.gsub /^/, '  '}"
  end

  def start
    @choices = ["- show answer/"]
    @pending = @completed.reduce([{}, 1]) do |acc, o|
      acc[0][acc[1]] = o[/ (.+)/, 1]
      acc[1] += 1
      acc
    end[0]

    # Shuffle (try up to 3 times if 1st one still at top)...

    length = @pending.length

    @progress = (1..length).to_a
    6.times{ @progress = @progress.sort_by{ rand } if @progress[0] == 1 }
    @progress = [@progress, []]

    @completed = ["| #{@pending[@progress[0][0]].sub(/ : .+/, ' : ___')}"]

    serialize
  end

  # Propagates any changes to current line to the hidden original below.
  # Smart enough to handle "foo : bar" or "foo : ?".
  def propagate_edits

    active = @completed[-1]

    # If ends with ": ___", just replace with what's before it

    update_this = @pending[@progress[0][0]]
    if active =~ /\| (.+) : ___$/
      txt = $1
      update_this.sub! /.+? : /, "#{txt} : "
    else
      update_this.replace active[/\| (.+)/, 1]
    end
  end

end
