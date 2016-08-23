class Timer
  def self.menu started=nil

    # If no time passed, just output current

    if ! started
      return "- started: " + Time.now.strftime("%I:%M:%S%p/").downcase
    end

    elapsed = Time.now - Time.parse(started)
    elapsed = elapsed.to_i
    seconds = (elapsed % 60).to_s
    elapsed = "#{elapsed / 60}:#{seconds.sub /^.$/, "0\\0"}"

    "| #{elapsed} elapsed"

  end
end
