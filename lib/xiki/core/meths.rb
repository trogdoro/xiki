# Is this still being used?

gem 'method_source'
require 'method_source'

# Adds .drill method to Object and Class

# For:
#   my_instace.drill

class ::Object
  #   def drill method=nil

  #     # If no method, show all
  #     if method.nil?
  #       result = ""
  #       methods = self.methods - Object.methods
  #       cmethods = self.class.methods - Class.methods
  #       return methods.sort.map{|o| "#{o}/"} + cmethods.sort.map{|o| "self.#{o}/"}
  #     end

  #     # Method passed

  #     method

  #     # CodeTree.tree_search_option + result
  #   end

  def self.meths method=nil
    Class.meths_internal self, method
  end

  def meths method=nil
    Class.meths_internal self.class, method
  end

  def self.meths_internal clazz, method=nil
    Meths.list clazz, method
  end
end

class Meths

  def self.list clazz, method=nil

    clazz = Xiki.const_get(clazz) if clazz.is_a?(String)

    # Foo., so list all methods...

    if method.nil?
      result = ""
      methods = clazz.instance_methods - Object.methods
      cmethods = clazz.methods - Class.methods
      return cmethods.sort.map{|o| "#{o}"} + methods.sort.map{|o| "##{o}"}
    end

    # Foo.bar, so run or navigate...

    method.sub! /\/$/, ''

    if method =~ /#(.+)/   # If instance method, always navigate
      return ""
    end

    # If as+open, jump to it...

    if Xiki::Keys.prefix == "open"
      file, line = clazz.method(method).source_location
      Xiki::View.open file, :to=>line
      return ""
    end

    # Run it...

    returned, out, exception = Xiki::Code.eval "#{clazz}.#{method}", Xiki::View.file, Xiki::View.line, :pretty_exception=>1
    return exception if exception
    returned ||= out   # Use output if nothing returned
    returned = returned.to_s if returned
    returned

  end

end
