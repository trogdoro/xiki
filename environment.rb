class Environment
  extend ElMixin
  def self.linux?
    elvar.system_type.to_s[/linux/]
  end
end
