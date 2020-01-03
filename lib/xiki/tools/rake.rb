module Xiki
  class Rake
    def self.menu task=nil

      if task.nil?   # If no task passed, show all
        txt = Shell.run 'rake -T', :sync=>true
        return "- Error: no rake file." if txt =~ /^No Rakefile found/
        return txt.scan(/^rake ([\w:]+)/).join("\n")
      end

      Shell.run "rake #{task}"   # If task passed, run it
    end
  end
end
