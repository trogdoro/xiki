class TxtHandler
  def self.handle options, ex
    return if ! ex['txt'] || options[:output] || options[:halt]

    txt = File.read "#{options[:last_source_dir]}#{ex['txt']}"

    if options[:client] =~ /^web\//
      txt = "<pre>#{txt}</pre>"
      return options[:output] = txt
    end

    options[:output] = Tree.quote txt
  end
end
