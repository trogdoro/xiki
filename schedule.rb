class Schedule
  def self.menu
    puts "$td"
    txt = nil
    $el.with(:save_window_excursion) do
      View.open("$td")
      txt = View.txt
    end
    puts txt.grep(/^\| e /).join("").gsub(/^/, '  ')
  end
end
