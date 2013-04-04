class HtmlHandler
  def self.handle options, ex
    return if ! ex['html'] || options[:output]
    options[:output] = File.read "#{options[:last_source_dir]}#{ex['html']}"
  end
end
