require "xiki/pre_pattern"

class Pattern
  @@defs ||= {}

  def self.defs
    @@defs
  end

  # Adds hash to :expanders for each pattern that matches.
  def self.expands? options, defs_override=nil
    defs_override ||= @@defs   # Will be overridden by PrePattern

    defs_override.each do |key, value|

      if key == :global   # Treat :global differently
        value.each do |regex, implementation|
          self.add_expander_maybe regex, implementation, options
        end
      else   # Some other key, which might be in options
        if nested = value[options[key]]
          # In next version, possibly recurse here (but extract out first so we're not in .expands?)

          # Assume it's //=>Proc for now
          # Loop through keys and add expander if proc
          nested.each do |regex, implementation|
            self.add_expander_maybe regex, implementation, options
          end

        end
      end
    end
    nil

    # TODO: When we need nesting, refactor to iterate through tree
    #
    # - alternate implementation
    #   - try to abstract into Xi.dig
    #     - a generic method that will climb in and return matches
    #       - based on a target_hash where
    #       - the tree will be climbed if keys/vals at any level are keys/vals in target_hash
    #     - example:
    #       - Xi.dig hash, target
    #       - Xi.dig {:word=>{"select"=>{:extension=>{"notes"=>'$'}}}}, {:extension=>{"notes"}, :word=>"select"}
    #         - returns => array with "$" in it
    #
    # - Step through tree while..
    #   - for each key
    #     - if a regex
    #       - add value (a proc) if the regex matches :path
    #     - if a symbol
    #       - look up value for that key, based on options[key] (the value in options using that same key)
    #         - if found, procede to parse its value, which is hash of symbols/regex's
    #           - do recursively?
    #             - probably
    #           - or: store a stack of .keys output and current index for each level
    #             | stack => [[:target_view, :global], [/foo/, :extension]]
    #             | stack_indexs = [0, 1]
    # - Treat :global differently

    # Example of what's in @@defs
    #   | :target_view => {
    #   |   "*ol" => {
    #   |     /foo/         => #<Proc:0x007f9e9cd68318@/docs/todo/todo.notes:61>,
    #   |     :extension => {
    #   |       "rb" => {
    #   |         // => #<Proc:0x007f9e9cb36608@/docs/todo/todo.notes:76>
    #   | }}}},
    #   | :global      => {
    #   |   /hi/ => #<Proc:0x007f9e9cb21a28@/docs/todo/todo.notes:78>
    #   | }

  end

  def self.add_expander_maybe regex, implementation, options
    match = regex.match options[:path]
    return if ! match

    (options[:expanders] ||= []).push(expander = {:expander=>Pattern})
    expander[:match] = match
    expander[:regex] = regex
    if implementation.is_a? Proc
      expander[:proc] = implementation
    elsif implementation.is_a? Class
      expander[:class] = implementation
    end
  end

  def self.expand options

    expander = options[:expanders][options[:expanders_index]]
    options[:no_slash] = 1   # For menus, no slash by default, though they can remove the option

    if options[:path] && expander[:proc]
      begin
        output = expander[:proc].call(options[:path], options)
        options[:output] = output if output
        return
      rescue Exception=>e
        return options[:output] = CodeTree.draw_exception(e, expander[:proc].source)
      end

    end

    raise "Don't yet know how to handle pattern for something with an implement other than a proc: #{options.inspect}"
  end

end


# TODO: Unified > probably move this to ~/xiki/... somewhere
# Pattern.default_patterns
