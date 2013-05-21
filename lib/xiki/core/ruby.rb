module Xiki
  class Ruby
    def self.menu
      "
      - .classes/
      - @eval/
      - @technologies/ruby/
      - links/
        > Core docs
        @http://ruby-doc.org/core-1.9.3/
        > Which gems are hot by categories
        @https://www.ruby-toolbox.com/
      - see/
        > Interactive docs
        @ri/
      "
    end

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

    # Ruby mode shortcuts custom+next and custom+previous
    #
    # To use them, add this line:
    # ~/.el4r/init.rb
    #   | Ruby.keys
    def self.keys
      Keys.custom_next(:ruby_mode_map) {
        column = View.column
        Move.to_end
        Search.forward "^ *\\(def\\|it\\) ", :beginning=>1, :go_anyway=>1
        View.column = column
      }

      Keys.custom_previous(:ruby_mode_map) {
        column = View.column
        Move.to_axis
        Search.backward "^ *\\(def\\|it\\) ", :go_anyway=>1
        View.column = column
      }
    end

    # Makes "Foo.bar" string from quoted method line.
    def self.quote_to_method_invocation txt=nil
      path ||= Tree.construct_path
      path.sub(/.+?(\w+)\.rb.+def (self\.)?([\w?]+).*/){"#{TextUtil.camel_case $1}.#{$3}"}
    end

  end
end
