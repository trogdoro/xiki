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
Ol.line
#
Ol << "self: #{self.inspect}"
    Class.meths_internal self, method
  end

  def meths method=nil
Ol.line
    Class.meths_internal self.class, method
  end

  def self.meths_internal clazz, method=nil
Ol << "clazz: #{clazz.inspect}"
    # If no method, show all
    if method.nil?
Ol.line
      result = ""
      methods = clazz.instance_methods - Object.methods
      cmethods = clazz.methods - Class.methods
      return cmethods.sort.map{|o| ".#{o}/"} + methods.sort.map{|o| "##{o}/"}
    end

    # Method passed

    method.sub! /\/$/, ''
    if method =~ /\.(.+)/
Ol << "method: #{method.inspect}"
      return "- " + clazz.method($1).source_location.join(':')
    end
Ol.line
    "- " + clazz.instance_method(method).source_location.join(':')

  end

  #     result = ""

  #     methods.sort.each do |m|
  #       location = self.instance_method(m).source_location.join ':'

  #       result << "- #{m}: #{Bookmarks.collapse location}\n"
  #     end

  #     cmethods = self.methods - Class.methods
  #     cmethods.each do |m|
  #       location = self.method(m).source_location.join ':'
  #       result << "- self.#{m}: #{Bookmarks.collapse location}\n"
  #     end

  #     #     # CodeTree.tree_search_option + result
  #     result
  #   end

end
