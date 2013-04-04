class TxtHandler
  def self.handle options, ex
    return if ! ex['txt'] || options[:output]

    txt = File.read "#{options[:last_source_dir]}#{ex['txt']}"

    if options[:client] == :web
      txt = "<pre>#{txt}</pre>"
    end

    options[:output] = txt
  end
end
