class PythonHandler
  def self.handle options, ex
    return if ! ex['py'] || options[:output]

    # Better api call for this?  Maybe pass just :source into options?
    # And maybe pass 'ex' in through options as well, so we don't need the param.

    source = "#{options[:last_source_dir]}#{ex['py']}"
    txt = Console.run "python \"#{source}\"", :sync=>true
    options[:output] = txt
  end
end
