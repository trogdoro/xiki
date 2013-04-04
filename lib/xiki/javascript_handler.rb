class JavascriptHandler
  def self.handle options, ex
    return if ! ex['js'] || options[:output]

    source = "#{options[:last_source_dir]}#{ex['js']}"
    txt = Console.run "js \"#{source}\"", :sync=>true
    options[:output] = txt
  end
end
