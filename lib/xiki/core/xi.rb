# Provides the Xi class and the Xi() method.
# The Xi() methad is experimental, and attempts to stringify a class



# Stringifies an object, sort of like .to_yaml.
# Needs improvement.

# Sample usage:
#
# x = Xi.new "
#   fish/
#     tuna/
#     shark/
#   mammals/
#   "
#
# puts x['fish']
#   tuna/
#   shark/
#
# puts x['']
#   fish/
#   mammals/

def Xi object, options={}
  Xiki::Xi.xi object, options
end

module Xiki
  class Xi

    MENU = %`
      - docs/
        - summary/
          | Implements the "Xi" data structure.  Kind if like haml
          | or json, but looks like the Xiki menu format (2-space
          | indenting and slashes at the end)
          - quick example/
            | You can make a new Xi object from a tree string, like this:
            @code/
              | foo = Xi.new("a/\\n  b/")
              | # Then you can query it by passing a path.
              | p foo[""]  # Returns "a/", because it's at the root
              | p foo["a"]  # Returns "b/", because it's nested under "a"
        - limitations/
          | Note that Xi has only a simple subset of Xiki's menu
          | functionality.  It's meant for when you need only a simple
          | data structure.
        - Xi method/
          | There's an experimental Xi() method, which wraps Xi.xi().
          | You pass it a data structure (nested hashes and
          | arrays) and it shows a Xi-ish representation of a data.
          | But it is currently unfinished and inconsistent.
          - examples/
            @! Xi("a")
            @! Xi("a\\nb")
            @! Xi(["a", "b"])
            @! Xi("a"=>"aa", "b"=>"bb")
            @! Xi(["a", ["bb", "cc", {"a"=>"aa", "b"=>"bb"}]])
          - status/
            | Unfinished...
          - ideas/
            | Ideas for improving Xi() in the future.  Since the "- /"
            | and "- {/" syntax are kind of crappy.
            |
            | - Ways to indicate whether hash or list?
            |   - Different kind of bullets, maybe?
            |     | =
            |   - Different kind of suffix?
            |     | :
        - utility methods/
          | These two methods give you shortcuts for deeply setting values
          | in nested hashes, or deeply retreiveing values from nested hashes.
          - hset/
            @! Xi.hset({:x=>:y}, :a, :b, :c)
          - hget/
            @! Xi.hget({:a=>{:b=>:c}}, :a, :b)
      `

    # Xi.hset({:x=>:y}, :a, :b, :c)
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

    # Xi.hget({:a=>{:b=>:c}}, :a, :b)
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

    def inspect
      @txt
    end

    def self.xi object, options={}
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
          self.xi v, options
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
          self.xi i, options
        end
        options[:indent] -= 1
        return options[:txt]
      end

      options[:txt]
    end


  end
end
