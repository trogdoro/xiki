require 'uri'

class Memorize

  include Xiki   # To have View class, etc.

  MENU = "
    > Example
    | France : Paris
    | England : London
    | Japan : Tokyo
    | Germany : Berlin
    "
  # - docs/
  #   | Add some facts like the example, and launch one of the
  #   | example lines to start an interactive memorize process.
  #   | In addition to helping you memorize, this is a low-stress
  #   | way to digest and review facts.
  #   | Double-click on one of to lines to begin.

  MENU_HIDDEN = %`
    + .show answer
    + .I was wrong
    + .I was right
    `

  MENU_OBSCURED = %`
    - .test/
      - deserialize/
        : > Example
        : | Germany : Berlin
        : | China : Beijing
        : | Japana : ___
        : + show answer
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
        :     "+ show answer"
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

    pending = @pending.map{|k, v| "|*#{k} #{v}"}

    progress = @progress.any? ? ["|*#{JSON[@progress]}"] : []

    txt = @heading + @completed + @choices + progress + pending
    txt = txt.map{|o| "#{o}\n"}.join("")
    txt
  end

  def deserialize txt
    txt = txt.split "\n"

    @heading, @completed, @choices, @progress, @pending = [], [], [], nil, nil

    @heading << txt.shift while txt.any? && txt[0] !~ / : /
    @completed << txt.shift while txt.any? && txt[0] =~ / : /
    @choices << txt.shift while txt.any? && txt[0] =~ /^\+/

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
    @choices = ["+ I was wrong", "+ I was right"]

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


  def self.i_was_right txt=nil
    return "=beg/neighbors/" if ! txt   # Beg for consecutive lines if not passed yet

    o = self.new txt
    txt = o.i_was_right
    o.set_line_found yield

    "<:\n#{txt.gsub /^/, '  '}"
  end

  def i_was_right
    propagate_edits

    @choices = ["+ show answer"]

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
    @completed.each_with_index{|item, i|
      result[@progress[1][i]-1] = item
    }
    @completed = result
  end

  def self.menu_before *args

    # Dropdown and memorizing in place is disabled for now,
    # until I re-write it (making text hidden doesn't work any more).

    # Only do something if right-click...

    task = yield[:task]
    return if ! task

    # /, so show "start with an example"?...

    # /some content, so offer to save to memorize.com...

    if args[0] =~ /\n/ # || args[0] =~ /^\|/
      return "
        ~ memorize
        ~ learn on Memorize.com
        " if task == []

      if task == ["learn on Memorize.com"]
        return self.browser args[0]
      end

      # If task is "~memorize", it'll just continue on as normal expand, and start

    end

    nil

  end

  def self.menu_after output, *path

    # We only want to interject if they're starting out,
    # which means launching a quoted line that got turned into linebreaks
    return if path.length != 1

    return if output   # Always return if there was output, because it was handled

    txt = path[0]

    o = self.new txt
    txt = o.start
    o.set_line_found yield

    "<:\n#{txt.gsub /^/, '  '}"
  end

  def start
    @choices = ["+ show answer"]
    @pending = @completed.reduce([{}, 1]) do |acc, o|
      acc[0][acc[1]] = o#[/ (.+)/, 1]
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

  # They right-clicked on a "a : b" table at the left margin

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
    "<! opened in browser"
  end

end
