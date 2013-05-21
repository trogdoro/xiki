class Environment

  # Will return either :osx, :linux, :windows, or :unknown.
  # Environment.os
  def self.os

    return :osx if RUBY_PLATFORM =~ /darwin/i
    return :unix if RUBY_PLATFORM =~ /linux|solaris/i
    return :windows if RUBY_PLATFORM =~ /mswin/i

    #     if RUBY_PLATFORM =~ /darwin/i
    #       { :os => "unix", :implementation => "macosx" }
    #     elsif RUBY_PLATFORM =~ /linux/i
    #       { :os => "unix", :implementation => "linux" }
    #     elsif RUBY_PLATFORM =~ /cygwin/i
    #       { :os => "unix", :implementation => "cygwin" }
    #     elsif RUBY_PLATFORM =~ /mingw/i
    #       { :os => "win32", :implementation => "mingw" }
    #     elsif RUBY_PLATFORM =~ /mswin/i
    #       { :os => "win32", :implementation => "mswin" }
    #     elsif RUBY_PLATFORM =~ /solaris/i
    #       { :os => "unix", :implementation => "solaris" }
    #     else
    #       { :os => "unknown", :implementation => "unknown" }

    :unknown
  end

  def self.current_emacs_path
    # This is only currently implemented for the mac.
    # Someone, make this work for Linux also.

    result = `ps -ef`[/ (\/.+\/(Aquamacs|Aquamacs Emacs|Emacs)\.app)/, 1]
  end
end
