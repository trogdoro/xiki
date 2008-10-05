require "location"
class App
#  extend KeyMappableMixin
  ## TODO - delete this

  include ElMixin
  extend ElMixin
  def self.open path
    Location.go path
  end
  def self.line
    line_number_at_pos
  end

  def self.enter_directory
    insert read_directory_name("Directory to insert: ", "/")
  end

  def self.enter_date
    insert elvar.current_prefix_arg ?
      Time.now.strftime("%Y-%m-%d %I:%M%p").sub(' 0', ' ') :
      Time.now.strftime("%Y-%m-%d")
  end
  def self.enter_from_difflog
    Location.as_spot
    DiffLog.open
    isearch_backward
  end

end
