module Xiki
  class NotesHandler
    def self.handle options
      source = options[:handlers]['notes'] || options[:handlers]['xiki']

      return if ! options[:handlers] || options[:output] || options[:halt]
      path = "#{options[:enclosing_source_dir]}#{source}"

      args = options[:args]||[]

      # If as+open, only pass in one line

      args[-1] = options[:one_line] if options[:prefix] == "open" && options[:one_line]

      txt = Notes.drill path, *args, options.select{|key, value| [:prefix, :task].include?(key)}

      options[:output] = txt
      options[:halt] = 1   # Just in case there's no output
    end
  end
end
