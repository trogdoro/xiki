class Schedule
  def self.menu
    puts "$t"
    txt = nil
    $el.with(:save_window_excursion) do
      View.open("$t")
      txt = View.txt
    end
    puts txt.grep(/^\| e /).join("").gsub(/^/, '  ')
  end
end
