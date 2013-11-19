module Xiki
  class StepsHandler
    def self.handle options

      source = options[:ex]['steps']

      return if ! options[:ex] || options[:output] || options[:halt]
      path = "#{options[:enclosing_source_dir]}#{source}"

      options[:halt] = 1   # Just in case there's no output

      txt = File.read path

      args = options[:args]

      sections = txt.split /^>.*\n/
      sections.shift   # First is blank
      sections.each{|o| o.strip!}

      options[:no_slash] = 1
      options[:no_search] = 1

      # /, so let drill show the first...

      if ! args
        return options[:output] = sections[0]
      end

      # /foo, so find item and use one after...

      # Get all lines
      return options[:output] = "@beg/siblings/" if args[-1] !~ /\n/

      section = args[-1].strip

      i = sections.index{|o| o =~ /\A#{Regexp.quote section}\z/}
      return options[:output] = "@flash/- slash :(!" if ! i


      i += options[:prefix] == :u ? -1 : 1

      following = sections[i]
      return options[:output] = "@flash/- end!" if ! following

      options[:output] = "@instead/siblings/\n#{sections[i].gsub(/^/, '  ')}"

    end
  end
end
