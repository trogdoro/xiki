# For now, Stringifies an object, sort of like .to_yaml.
# Needs improvement.
def Xi object, options={}
  options[:indent] ||= 0
  options[:txt] ||= ""
  # If string
  if object.is_a? String
    # if single line, just return with bullet
    if object !~ /\n/
      options[:txt] << "#{'  ' * options[:indent]}- #{object}\n"
      return options[:txt]
    else   # else if multiline, return with pipe quoting
      options[:txt] << object.gsub(/^/, "#{'  ' * options[:indent]}| ").gsub(/^( *)\| $/, "\\1|")
      return options[:txt]
    end
  end

  # If hash, for each key, print key and recurse on value
  if object.is_a? Hash
    options[:txt] << "#{'  ' * options[:indent]}- {/\n"
    options[:indent] += 1   # Add 2 spaces, but only for this recursive call
    object.each do |k, v|
      options[:txt] << "#{'  ' * options[:indent]}- #{k}/\n"
      options[:indent] += 1   # Add 2 spaces, but only for this recursive call
      Xi v, options
      options[:indent] -= 1
    end
    options[:indent] -= 1
    return options[:txt]
  end

  # If array, for each item, print string and recurse
  if object.is_a? Array
    options[:txt] << "#{'  ' * options[:indent]}- /\n"

    options[:indent] += 1   # Add 2 spaces, but only for this recursive call
    object.each do |i|
      Xi i, options
    end
    options[:indent] -= 1
    return options[:txt]
  end

  options[:txt]
end

module Xiki
  class Xi

    # p Xi.hset({:x=>:y}, :a, :b, :c)
    #   {:x=>:y, :a=>{:b=>:c}}
    def self.hset h, *array
      ancestors = array.dup
      key, value = ancestors.slice!(-2..-1)
      last = h
      ancestors.each do |o|
        # If hash not there yet, create new
        last = (last[o] ||= {})
        # Make reference be it
      end

      # Add value to end (last item in array)

      last[key] = value
      h
    end

    # p Xi.hget({:a=>{:b=>:c}}, :a, :b)
    #   :c
    def self.hget h, *array
      last = h
      array.each do |o|
        last = last[o]
        return nil if last.nil?
      end

      last
    end

    # Wraps a xiki tree.  Kind of like a hash.  Just wraps text
    # and passes to .children method.
    attr_accessor :txt
    def initialize txt=nil
      @txt = txt
      @txt.unindent! if txt =~ /\A\s/
    end

    def [] path
      return nil if ! @txt
      result = Tree.children @txt, path
      result.strip! if result
      result
    end

    def to_s
      @txt
    end

  end
end
