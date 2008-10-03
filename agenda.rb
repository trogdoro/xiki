gem 'googlecalendar'
require 'googlecalendar'

class Agenda
  extend ElMixin

  def self.username=(username)
    @username = username
  end

  # true if logged in, false otherwise
  def self.google_account
    return @agenda if @agenda
    unless @username
      View.message "add in init.rb: Agenda.username = 'yourname@gmail.com'"
      return
    end
    agenda = GData.new
    passwd = read_passwd "enter calendar password: "
    @agenda = agenda if agenda.login(@username, passwd)
  rescue
    false
  end

  def self.quick_add_line(msg = nil)
    return "Invalid login" unless self.google_account
    msg ||= Line.value.strip.sub(/^[\w-]+/, '')
    @agenda.quick_add(msg)
    Effects.blink
  end

  def self.load_today
    data = scan '/calendar/ical/french@holiday.calendar.google.com/public/basic'
    calendar = parse data
    text calendar, 'results.txt'
  end

  def self.growlnotify_in(minutes, msg = "Time is up!")
    Thread.new(minutes) do |min|
      sleep(min*60)
      `growlnotify -m'time is up'`
    end
  end
end


