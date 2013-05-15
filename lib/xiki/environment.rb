class Environment

  # Will return either :osx, :linux, :windows, or :unknown.
  def self.os

    # TODO: find a better way to do this
    return :osx if File.directory?("/Applications/")
    return :linux if File.directory?("/etc/")

    :unknown
  end

  def self.current_emacs_path
    # This is only currently implemented for the mac.
    # Someone, make this work for Linux also.

    result = `ps -ef`[/ (\/.+\/(Aquamacs|Aquamacs Emacs|Emacs)\.app)/, 1]
  end
end
