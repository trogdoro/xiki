module Xiki
  class Snippet
    def self.insert

      line = Line.value

      if ! line.blank?
        Keys.prefix = "source"
        Launcher.launch
        return
      end

      View << "@" if Line =~ /^ /

      Launcher.insert("snippet/")   # Until we port to unified
    end
  end
end
