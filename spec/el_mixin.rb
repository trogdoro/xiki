class Elvar
  def method_missing(func, *args, &block)
    # Do nothing
  end
end

$el = Elvar.new

module ElMixin
  def elvar
    Elvar.new
  end
  def method_missing(func, *args, &block)
    # Do nothing
  end
end


