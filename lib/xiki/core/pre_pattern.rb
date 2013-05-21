# Probably eventually merge this with Pattern
# - Pattern may want to fire on these things, like we do...
#   - target view name
#   - target file name
#   - just the above things, and no pattern
class PrePattern

  # Structure is something like...
  # :view=>{
  #   "*ol"=><Block>
  @@defs ||= {}

  def self.defs
    @@defs
  end

  def self.expands? options
    Pattern.expands? options, @@defs
  end

  def self.expand options
    # This method shouldn't be called - expander will intercepted by Pattern called
    # So just delete this method?
  end
end
