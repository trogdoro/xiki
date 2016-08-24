module Xiki
  class NotesHandler
    def self.handle options

      source = options[:handlers]['notes'] || options[:handlers]['xiki']
      return if ! options[:handlers] || options[:output] || options[:halt]
      path = "#{options[:enclosing_source_dir]}#{source}"

      args = options[:args]||[]

      # If as+open, only pass in one line

      args[-1] = options[:one_line] if options[:prefix] == "open" && options[:one_line]

      # Ctrl+X pressed, so just navigate
      if options[:ctrlx]
        options[:task] = ["view source"]
      end

      options_in = options.select{|key, value| [:prefix, :task, :items, :ancestors].include?(key)}
      txt = Notes.drill path, *args, options_in

      # Expand action

      file = ""

      if options_in[:heading_found] =~ /^> \./

        txt = Tree.unquote txt

        # Remove top level from :items?

        options_in[:items].shift

        txt = TopicExpander.expand_action(txt, file, options_in)   #> |
      end

      options[:output] = txt
      options[:halt] = 1   # Just in case there's no output
    end
  end
end
