class TxtHandler
  def self.handle options
    source = options[:ex]['txt']
    return if ! source || options[:output] || options[:halt]

    txt = File.read "#{options[:last_source_dir]}#{source}"

    if options[:client] =~ /^web\//
      txt = "<pre>#{txt}</pre>"
      return options[:output] = txt
    end

    options[:output] = Tree.quote txt
  end
end
