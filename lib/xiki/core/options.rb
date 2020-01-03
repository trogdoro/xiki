module Xiki
  class Options

    def self.propagate_some_outward options, options_out

      # Pass options from child up the parent stack

      keys_out = self.important_options_out

      keys_out.each do |key|
        options_out[key] = options[key] if options[key]
      end

    end

    def self.propagate_some_inward options, options_in

      # Pass options from here to the child call

      options.each{|k, v| options_in[k] = v if [
        :task,
      ].include?(k)}

    end


    #
    # xiki api/propagate only certain important options
    #
    def self.propagate_important_options options_outer

      # important_options = [:ctrlx, :task]
      options_in = self.important_options_in
      options_out = self.important_options_out

      options = options_outer.select{|key, value| important_options_in.include?(key)}

      result = yield(options)

      important_options_out.each do |key|
        options_outer[key] = options[key] if options[key]
      end

      result

    end

    def self.important_options
      [
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
    end

    def self.important_options_in
      [
        :task,
        :ctrlx,
        # :go,
      ]
    end

    def self.important_options_out
      [
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
        :returned_file_contents,
        # :go,
      ]
    end

  end
end
