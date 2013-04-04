class Pattern
  @@defs ||= {}

  def self.defs
    @@defs
  end

  def self.expands? options
    path = options[:path]

    # TODO: might be other key?

    @@defs.each do |regex, implementation|
      if match = regex.match(path)
        options[:match] = match
        options[:regex] = regex
        if implementation.is_a? Proc
          options[:proc] = implementation
          return true
        elsif implementation.is_a? Class
          options[:class] = implementation
          return true
        end
      end
    end
    nil
  end

  def self.expand options
    return options[:proc].call(options[:path], options) if options[:path]

    raise "Don't yet know how to handle pattern for something with an implement other than a proc: #{options.inspect}"
  end


  # Defines all the default patterns.  $... for shell commands, etc.
  def self.default_patterns

    Xiki.def(/^([$%&]) (.+)/) do |path, options|
      options[:no_slash] = 1
      prompt, command = options[:match][1..2]
      options[:command] = command

      Console.shell_command_per_prompt prompt, options
    end
  end
end
Pattern.default_patterns
