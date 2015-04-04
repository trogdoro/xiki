module Xiki
  class Options

    def self.propagate_some options_child, options

      # Pass options from child up the parent stack

      options_child.each{|k, v| options[k] = v if [
        :nest,
        :no_task,
        :no_slash,
        :no_search,
        :line_found,
        :hotkey,
        :omit_slashes,
        :error,
      ].include?(k)}

    end


  end
end
