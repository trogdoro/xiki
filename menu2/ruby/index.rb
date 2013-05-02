module Xiki::Menu
  class Ruby

    def self.classes clazz=nil, method=nil
      # /classes/, so show list of classes...

      if clazz.nil?
        result = []
        ObjectSpace.each_object(Class) do |c|
          name = c.to_s
          next if name =~ /^#/
          result << "- #{name}/\n"
        end
        return result.sort.join
      end

      # /classes/Class, so show methods...

      if method.nil?
        result = ""
        result << Kernel.const_get(clazz).instance_methods(false).sort.
          collect {|i| "- #{i}/" }.join("\n")
        result << Kernel.const_get(clazz).methods(false).sort.
          collect {|i| "- ::#{i}/" }.join("\n")
        return result
      end

      # /classes/Class/method, so lookup method's doc

      method = "##{method}" unless method =~ /^::/
      command = "ri --format=rdoc #{clazz}#{method}"
      Console[command].gsub(/\C-[.+?m/, '').gsub(/^/, '| ').gsub(/^\| +$/, '|')
    end
  end
end
