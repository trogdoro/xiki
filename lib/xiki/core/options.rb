module Xiki
  class Options

    def self.propagate_some_outward options_child, options

      # Pass options from child up the parent stack

      options_child.each{|k, v| options[k] = v if [
        :nest,
        :no_task,
        :no_slash,
        :no_search,
        :line_found,
        :column_found,
        :hotkey,
        :omit_slashes,
        :error,
        :filter_dont_collapse,
        :filter_number_counts_only_quotes,
        :leave_trailing,
        :digit_means_replace_parent,
      ].include?(k)}

    end

    def self.propagate_some_inward options, options_child

      # Pass options from here to the child call

      options.each{|k, v| options_child[k] = v if [
        :task,
      ].include?(k)}

    end


    #
    # xiki api/propagate important options
    #
    def self.propagate_important_options options_outer

      # important_options = [:ctrlx, :task]
      important_options = [
        :nest,
        :no_task,
        :no_slash,
        :no_search,
        :line_found,
        :column_found,
        :hotkey,
        :omit_slashes,
        :error,
        # :filter_not_recursive,
        :filter_dont_collapse,
        :filter_number_counts_only_quotes,
        :leave_trailing,
        :digit_means_replace_parent,
        :ctrlx,
        :task,
        # :go,
      ]

      options = options_outer.select{|key, value| important_options.include?(key)}

      result = yield(options)

      important_options.each do |key|
        options_outer[key] = options[key]
      end

      result

    end

  end
end
