class Environment
  def self.linux?
    $el.elvar.system_type.to_s[/linux/]
  end
end
