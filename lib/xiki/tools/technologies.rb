module Xiki
  class Technologies
    def self.menu topic=nil, *args

      heading, content = args.partition{|o| o !~ /^\|/}

      content = content.any? ? content.join("/") : nil
      heading = heading.any? ? heading.join("/") : nil

      # If no topic, just show all dirs

      if topic.nil?
        entries = Dir.new(Bookmarks[":te"]).entries
        entries = entries.select{|o| o =~ /^\w/}
        return entries.map{|o| "#{o}/"}
      end

      # If just topic, list all headings

      Notes.drill ":te/#{topic}/#{topic}.notes", heading, content

    end
  end
end
