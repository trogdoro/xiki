class Memorize

  include Xiki   # To have View class, etc.

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

  MENU_HIDDEN = %`
    - .show answer
    - .I was wrong
    - .I was right
    `

  MENU_OBSCURED = %`
    - .test/
      - deserialize/
        : > Example
        : | Germany : Berlin
        : | China : Beijing
        : | Japana : ___
        : - show answer
        : |*[[3,1,2],[]]
        : |*1 France : Paris
        : |*2 England : London
        : |*3 Japan : Tokyo
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
        :     "- show answer"
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
    require 'json'

    txt = ""

Ol "@pending", @pending   # => {1=>"France : Paris", 2=>"England : London", 3=>"Japan : Tokyo", 4=>"Germany : Berlin"}

# pending = @pending.map{|k, v| "|##{k} #{v}"}
    pending = @pending.map{|k, v| "|*#{k} #{v}"}

    progress = @progress.any? ? ["|*#{JSON[@progress]}"] : []

    txt = @heading + @completed + @choices + progress + pending
    txt = txt.map{|o| "#{o}\n"}.join("")
    txt
  end

  def deserialize txt
Ol.stack
    txt = txt.split "\n"
Ol "txt", txt
  # => ["| England : ___", "- show answer/", "|#[[2,4,3,1],[]]", "|#1 France : Paris", "|#2 England : London", "|#3 Japan : Tokyo", "|#4 Germany : Berlin"]
  # => ["| England : ___", "- show answer", "|#[[2,4,3,1],[]]", "|#1 France : Paris", "|#2 England : London", "|#3 Japan : Tokyo", "|#4 Germany : Berlin"]

    @heading, @completed, @choices, @progress, @pending = [], [], [], nil, nil

Ol "txt", txt   # => ["| England : ___", "- show answer/", "|#[[2,4,3,1],[]]", "|#1 France : Paris", "|#2 England : London", "|#3 Japan : Tokyo", "|#4 Germany : Berlin"]
    @heading << txt.shift while txt.any? && txt[0] !~ / : /
Ol "@heading", @heading   # => []
    @completed << txt.shift while txt.any? && txt[0] =~ / : /
Ol "@completed", @completed   # => ["| England : ___"]
    @choices << txt.shift while txt.any? && txt[0] =~ /^-/
Ol "@choices", @choices   # => ["- show answer"]

    @progress = txt.shift
Ol "@progress", @progress   # => "|#[[2,4,3,1],[]]"
    @progress = @progress ? JSON[@progress[/\[.+/]] : []

    @pending = txt
Ol "@pending", @pending   # => ["|#1 France : Paris", "|#2 England : London", "|#3 Japan : Tokyo", "|#4 Germany : Berlin"]

    @pending = @pending.reduce({}){|acc, o|
      match = o.match /(\d+) (.+)/
      acc[match[1].to_i] = match[2]
      acc
    }
Ol "@pending", @pending   # => {1=>"France : Paris", 2=>"England : London", 3=>"Japan : Tokyo", 4=>"Germany : Berlin"}

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
Ol["is this working?!"]
#Ol "options", options
    options[:no_search] = 1
    return if @progress[0].empty?
    options[:line_found] = @heading.length + @completed.length + 1
  end

  def self.show_answer txt=nil #, options={}
    return "=beg/neighbors/" if ! txt   # Beg for consecutive lines if not passed yet

    o = self.new txt
Ol "txt", txt   # => "show answer"
    txt = o.show_answer
    o.set_line_found yield
    # o.set_line_found options

    "<:\n#{txt.gsub /^/, '  '}"
  end

  def show_answer
Ol["!"]
    propagate_edits

    @completed[-1] = "| #{@pending[@progress[0][0]]}"
    @choices = ["- I was wrong", "- I was right"]

    serialize
  end


  def self.i_was_wrong txt=nil
    return "=beg/neighbors/" if ! txt   # Beg for consecutive lines if not passed yet

    o = self.new txt
    txt = o.i_was_wrong
    o.set_line_found yield

    "<:\n#{txt.gsub /^/, '  '}"
  end

  def i_was_wrong
    propagate_edits

    @completed.pop

    previous = @progress[0].shift
    @progress[0] << previous
    @progress[0].insert 2, previous if @progress[0].length > 1

    remove_runs

    @completed << "| #{@pending[@progress[0][0]].sub(/ : .+/, ' : ___')}"
    @choices = ["- show answer"]

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
    return "=beg/neighbors/" if ! txt   # Beg for consecutive lines if not passed yet

    o = self.new txt
    txt = o.i_was_right
    o.set_line_found yield

    "<:\n#{txt.gsub /^/, '  '}"
  end

  def i_was_right
    propagate_edits

    @choices = ["- show answer"]

    @completed.pop

    previous = @progress[0].shift
Ol "previous", previous

    # If that was the last one, show at top, and remember order
    if ! @progress[0].member? previous
      @completed << "| #{@pending[previous]}"
      # Remember its order, and remove its redundant value from @pending
      @progress[1] << previous
      @pending.delete previous
    end

    # If that was the last one, they're finished!...

    if @progress[0].empty?
      finished
      @choices = []
      @progress = []
      return serialize
    end

    # Show next question...

    @completed << "| #{@pending[@progress[0][0]].sub(/ : .+/, ' : ___')}"

    serialize
  end


  def finished
    result = []
Ol "@completed", @completed   # => ["| Japan : Tokyo", "| England : London", "| Germany : Berlin", "| France : Paris"]
Ol "@progress", @progress   # => [[], [3, 2, 4, 1]]
# @progress[1].each_with_index{|i, j| result[i-1] @completed[i-1]}
    @completed.each_with_index{|item, i|
Ol.>> item, i
      result[@progress[1][i]-1] = item
    }
Ol "result", result
    @completed = result
Ol "@completed", @completed   # => ["| Germany : Berlin", "| England : London", "| France : Paris", "| Japan : Tokyo"]
  end


  def self.menu_before *args

Ol "args", args   # => ["France : Paris\nEngland : London\nJapan : Tokyo\nGermany : Berlin\n"]

    # Only do something if right-click...

    dropdown = yield[:dropdown]
    return if ! dropdown

    Ol "dropdown", dropdown   # => ["save to Memorize.com"]

    # /, so show "start with an example"?...


    # /some content, so offer to save to memorize.com...

    if args[0] =~ /\n/ # || args[0] =~ /^\|/
      return "~ memorize\n~ save to Memorize.com" if dropdown == []
      if dropdown == ["save to Memorize.com"]
Ol["populate!"]
        return self.browser args[0]
      end

      # If dropdown is "~memorize", it'll just continue on as normal expand, and start

    end


Ol "args", args   # => ["London : Englandd\nPariss : France\nBerlinnn : Germany\nTokyoo : Japann\n"]
    nil


    # Ol "args", args   # => ["show answer", "| Japan : ___\n- show answer/\n|*[[3,4,2,1],[]]\n|*1 France : Paris\n|*2 England : London\n|*3 Japan : Tokyo\n|*4 Germany : Berlin\n"]
    # if args[0] == "show answer"
    #   # return self.show_answer *args, &yield
    #   return self.show_answer args[1], yield
    # end

    # nil
  end

  def self.menu_after output, *path

    Ol "output", output

    # We only want to interject if they're starting out,
    # which means launching a quoted line that got turned into linebreaks
    return if path.length != 1

    return if output   # Always return if there was output, because it was handled

    # |#, so beg for all lines
#    return "=beg/neighbors/" if path[0] =~ /\A[|>].+\z/

    txt = path[0]
Ol "txt", txt   # => "France : Paris\nEngland : London\nJapan : Tokyo\nGermany : Berlin\n"

    o = self.new txt
    txt = o.start
    o.set_line_found yield

    # "<:\n#{txt.gsub /^/, '  '}"
Ol["!"]
    "<:\n#{txt.gsub /^/, '  '}"
  end

  def start
Ol.stack
    @choices = ["- show answer"]
Ol "@completed", @completed   # => ["| England : ___"]
    @pending = @completed.reduce([{}, 1]) do |acc, o|
      # acc[0][acc[1]] = o[/ (.+)/, 1]
      acc[0][acc[1]] = o#[/ (.+)/, 1]
      acc[1] += 1
      acc
    end[0]

Ol "@pending", @pending   # => {1=>": Paris", 2=>": London", 3=>": Tokyo", 4=>": Berlin"}
    # Shuffle (try up to 3 times if 1st one still at top)...

    length = @pending.length
Ol "length", length   # => 0

    @progress = (1..length).to_a
    6.times{ @progress = @progress.sort_by{ rand } if @progress[0] == 1 }
    @progress = [@progress, []]
Ol "@progress", @progress   # => [[], []]

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

  # They right-clicked on a "a : b" table at the left margin

  def self.dropdown_save_to_memorize

    # Grab paragraph...

    txt = View.paragraph
Ol.a txt

    # Open in browser...

    self.browser txt

  end

  # They right-clicked on a "a : b" table at the left margin

  def self.dropdown_memorize

    # Grab paragraph...

    txt = View.paragraph
Ol.a txt

    txt = "memorize/\n#{txt.gsub(/^/, '  | ')}"
    View.open "memorize", :txt=>txt
    View.line = 2
    Launcher.launch

    # TODO > Cut off except final a:b lines (in case heading next to it)

Ol["!"]
    ""
  end

  def self.browser txt
    path = txt.gsub(" : ", ":").gsub("\n", "/").gsub(" ", "-")
    Browser.url "http://memorize.com/#{path}"
    "<! opened in browser"
  end

end
