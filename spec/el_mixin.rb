class Elvar
  def method_missing(func, *args, &block)
    # Do nothing
    print "B"
  end
end

module ElMixin
  def elvar
    Elvar.new
  end
  def method_missing(func, *args, &block)
    # Do nothing
    print "B"
  end
end
