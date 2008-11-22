class Processes
  def self.menu pid=nil
    if pid.nil?   # If nothing passed, show them
      lines = Console.run("ps auxc", :sync=>true).split("\n")[1..-1]
      lines.each do |l|
        f = l.split(/ +/)
        puts "- #{f[10]} - #{f[2]}%: #{f[1]}"
      end
      return
    end

    # pid passed, so kill process
    Console.run "sudo kill #{pid}"
  end
end
