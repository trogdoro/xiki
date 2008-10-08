class Ruby
  def self.menu
    puts "
      + .list_classes/
      - .re_index_fast_ri
      "

  end

  def self.list_classes clazz=nil, method=nil

    unless clazz   # Show list of classes
      ObjectSpace.each_object(Class) do |c|
        puts c
      end
      return
    end

    unless method   # Show list of methods
      puts Kernel.const_get(clazz).instance_methods(false).sort.
        collect {|i| "##{i}" }
      puts Kernel.const_get(clazz).methods(false).sort.
        collect {|i| "::#{i}" }
      return
    end

    # Lookup method doc
    puts `qri #{clazz}#{method}`.gsub(/\C-[.+?m/, '')
  end

  def self.re_index_fast_ri
    Console.run "fastri-server -b"
  end

end
