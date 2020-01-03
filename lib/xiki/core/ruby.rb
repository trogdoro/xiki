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
    def self.custom_next

      column = View.column
      Move.to_end
      Search.forward "^ *\\(def\\|it\\) ", :beginning=>1, :go_anyway=>1
      View.column = column

      Keys.remember_key_for_repeat(proc {Ruby.custom_next}, :movement=>1)
    end

    def self.custom_previous
      column = View.column
      Move.to_axis
      Search.backward "^ *\\(def\\|it\\) ", :go_anyway=>1
      View.column = column

      Keys.remember_key_for_repeat(proc {Ruby.custom_previous}, :movement=>1)
    end

    def self.init_in_client

      # Call it when ruby-mode-map

      $el.el4r_lisp_eval %`
        (progn
          (defun xiki-ruby-mode-keys ()
            (el4r-ruby-eval "Xiki::Ruby.keys")
          )
          (add-hook 'ruby-mode-hook 'xiki-ruby-mode-keys)

          ; Highlight "#+ Comments" indent as white (it wasn't possible to highlight the comment itself)
          (font-lock-add-keywords 'ruby-mode '(
            ("\\\\( *\\\\)#\\\\+" (0 nil) (1 'color-rb-white))
          ))

        )
      `

      self.keys   # Call it now, in case ruby-mode-map already loaded (it probably is if reloading after a .rb file was visited)

    end

    def self.keys
      if $el.boundp :ruby_mode_map

        $el.define_key(:ruby_mode_map, $el.kbd("C-c C-n")){ Ruby.custom_next }   # custom+next
        $el.define_key(:ruby_mode_map, $el.kbd("C-c C-p")){ Ruby.custom_previous }   # custom+previous

        $el.define_key :ruby_mode_map, $el.kbd("C-c C-e") do
          Xiki::View.insert "end"
          $el.ruby_indent_line
        end

      end

    end

    # Makes "Foo.bar" string from quoted method line.
    def self.quote_to_method_invocation txt=nil
      path ||= Tree.construct_path
      path.sub(/.+?(\w+)\.rb.+def (self\.)?([\w?]+).*/){"#{TextUtil.camel_case $1}.#{$3}"}
    end

  end
end
