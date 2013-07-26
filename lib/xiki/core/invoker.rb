module Xiki
  class Invoker
    # Invokes actions on menu source classes.
    #
    # The actioun is often the .menu method, but could alternateyl be handled
    # by the MENU constant or the foo.menu file.  Also, MENU or foo.menu may route
    # to a different method, in which case that will be invoked.
    #
    # Todo: abstract this out so it can invoke other languages too?
    # - Make it delegate back to RubyHandler for running stuff
    #   - Make it have specific methods, for each type of eval below:
    #     - handler.check_class_defined?
    def self.invoke clazz, args=[], options={}

      args ||= []
      args = Path.split args if args.is_a? String

      menu_found = nil   # Possible values: nil, :constant, :file, :method (if :method, it overwrites the previous value, which is fine)

      action_method = nil
      some_method_ran = false

      code, clazz_name, dot_menu_file = options[:code], options[:clazz_name], options[:dot_menu_file]


      # Prepare by loading or reloading class...

      # Just always reload for now (no caching)
      # Assume clazz is a file for now

      returned, out, exception = Code.eval code, clazz, 1, :global=>1
      return CodeTree.draw_exception exception, code if exception

      # Prepend module name if any...

      mod = self.extract_ruby_module code if code
      clazz_name = "#{mod}::#{clazz_name}" if mod

      clazz_name = "Xiki" if clazz_name == "Xiki::Xiki"   # Xiki is a module, so it grabbed it twice.  If we want to support other modules, do something generic here, but we may not need to.

      # Before this, 'clazz' is a file path
      clazz = Code.simple_eval("defined?(#{clazz_name}) ? #{clazz_name} : nil", nil, nil, :global=>1) if clazz_name

      # Call .menu_before if exists...

      method = clazz.method(:menu_before) rescue nil

      if method
        some_method_ran = true
        code = proc{ method.call(*args) {options} }
        returned, out, exception = Code.eval code

        return CodeTree.draw_exception exception, code if exception
        if returned
          returned.unindent! if returned =~ /\A[ \n]/
          return returned
        end
      end

      # Grab MENU constant or .menu file...

      if clazz.const_defined? :MENU
        menu_found = :constant
        menu_text = clazz::MENU
      elsif dot_menu_file && File.file?(dot_menu_file)
        menu_found = :file
        menu_text = File.read dot_menu_file
      end

      txt = nil

      # If MENU|foo.menu, do routing (get children or dotify)...

      dotified = []

      if menu_text

        menu_text = menu_text.unindent if menu_text =~ /\A[ \n]/
        txt = Tree.children menu_text, args, options

        # If there was output use it, otherwise try routing (dotifying)

        if ! txt || txt == "- */\n"
          dotified = Tree.dotify menu_text, args
        elsif menu_found == :constant || menu_found == :file   # If there was autput from MENU or foo.menu, eval !... lines
          MenuHandler.eval_when_exclamations txt, options
        end
      end


      # If MENU_OBSCURED exists, use it to get children or route...

      menu_obscured = clazz.const_defined? "MENU_OBSCURED"
      if menu_obscured
        menu_obscured = clazz.const_get "MENU_OBSCURED"
        menu_obscured = menu_obscured.unindent if menu_obscured =~ /\A[ \n]/

        txt_from_obscured = !txt && args.any? ?   # Only use children if it's not the root
          Tree.children(menu_obscured, args, options) : nil

        if txt_from_obscured
          txt = txt_from_obscured
        else
          dotified = Tree.dotify menu_obscured, args, dotified
        end
      end


      # If MENU_HIDDEN exists, use it to route...

      menu_hidden = clazz.const_defined? "MENU_HIDDEN"
      if menu_hidden
        menu_hidden = clazz.const_get "MENU_HIDDEN"
        menu_hidden = menu_hidden.unindent if menu_hidden =~ /\A[ \n]/

        # Try getting children from MENU_HIDDEN, if it has any...

        dotified = Tree.dotify menu_hidden, args, dotified
      end

      # If MENU|foo.menu not found or didn't handle path, call routed method or otherwise .menu with args...
      if ! txt
        # Figure out whether to try .menu or routed .method
        action, variables = self.actionify args, dotified

        menu_found = :method if clazz.method(action) rescue nil   # Method exists, so call it

        # If no menu or routed method of any kind, try menuless mode...

        if ! menu_found
          cmethods = clazz.methods - Class.methods
          options[:instance_method] = 1 if cmethods.empty?

          if args.empty?   # /, so list all methods
            # If MENU|foo.menu|Foo.menu, just show all methods, and call method based on name...

            # Pass instance methods if none
            cmethods = clazz.instance_methods - Class.instance_methods if cmethods.empty?

            txt = cmethods.sort.map{|o| "+ #{o}/\n"}.join ""

          else   # /foo/bar, so invoke /.foo/bar!
            # Artificially route to 1st item to be the action
            dotified = [true]   # Indicate only the first item is the action
            action, variables = self.actionify args, dotified
          end

        end

        # If still no text, try .menu or routed method...

        if ! txt
          action_method =
            if options[:instance_method]
              clazz.new.method(action) rescue nil
            else
              clazz.method(action) rescue nil
            end

          if action_method   # Method exists, so call it
            some_method_ran = true
            menu_found = :method

            # Call action...

            code = proc{ action_method.call(*variables) {options} }
            txt, out, exception = Code.eval code

            txt = CodeTree.returned_to_s(txt) if txt   # Convert from array into string, etc.
            txt = txt.unindent if txt =~ /\A[ \n]/

            if exception
              args = variables.map{|o| "\"#{CodeTree.escape o}\""}.join(", ")   # This was only when we eval'ed
              code = "#{clazz_name}.#{action} #{args}".strip
              return CodeTree.draw_exception exception, code
            end
          end
        end
      end

      # TODO: Be sure to eval output that starts with "! "
        # When?
          # Even when self.menu output?


      #     # TODO: Unified: comment out for now - just comment out since we're doing no caching
      #     # reload 'path_to_class'
      #     Menu.load_if_changed File.expand_path("~/menu/#{snake}.rb")

      # Call .menu_after if it exists...

      method = clazz.method(:menu_after) rescue nil
      if method
        some_method_ran = true

        code = proc{ method.call(txt, *args) {options} }
        returned, out, exception = Code.eval code

        return CodeTree.draw_exception exception, code if exception
        if returned
          returned.unindent! if returned =~ /\A[ \n]/
          txt = returned
        end
      end

      if ! txt
        return nil if some_method_ran   # Only say "no output" if didn't call .menu, .menu_before|_after, or other action

        # For now, let's try not doing this
        txt = "@flash/- no output!" if options[:client] =~ /^editor\b/
      end

      txt
    end


    # Breaks args down into a method ("action") and the params
    # Invoker.actionify(["act", "b"], [true])
    #   ["act", ["b"]]
    def self.actionify args, boolean_array

      # Last .dotted one is the action, and non-dotted are variables to pass
      i = -1
      actions, variables = args.partition{|o|
        i += 1
        boolean_array[i]
      }

      action = actions.any? ? actions[-1].dup : "menu"
      action.gsub! /[ -]/, '_'
      action.gsub! /[^\w.]/, ''

      action.downcase!

      [action, variables]
    end

    def self.extract_ruby_module txt

      txt = txt.sub /\A( *class .+?\n).+/m, "\\1"   # Remove everything after 1st class... line, so it doesn't look at internal irrelevant module statements in a script.

      # If it's class Foo::Bar, pull it out of there
      if mod = txt[/class (.+)::/, 1]
        return mod
      end

      txt = txt.scan(/^ *module (.+)/).map{|o| o[0]}.join("::")

      return nil if txt == ""
      txt
    end
  end
end
