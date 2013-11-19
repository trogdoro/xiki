class Timer
  def self.menu started=nil

    # If no time passed, just output current

    if ! started
      return "- started: " + Time.now.strftime("%I:%M:%S%p/").downcase
    end

    elapsed = Time.now - Time.parse(started)
    elapsed /= 60.0
    elapsed  = sprintf('%.2f', elapsed)

    "| #{elapsed} minutes elapsed"

  end
end
