def X *args
  Xiki.expand *args
end

class X
  def self.sh command, options={}
    Xiki::Shell.sync command, options
  end

  def self.to_s
    %`
      | The X method is a convenience wrapper around Xiki.expand()
      | Example usage:
      =X "ip"
    `
  end
end

