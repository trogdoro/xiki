class Path

  # Removes |... from path and returns it
  # Only reliable when one quote.
  #
  # p Path.extract_quote "/projects/foo/bar.rb/|  quoted"
  #   "  quoted"
  def self.extract_quote path
    return if ! found = path =~ %r"/\|(.+)"
    path.slice!(found..-1).sub(/../, '')
  end
end
