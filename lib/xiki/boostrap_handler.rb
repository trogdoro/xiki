class BoostrapHandler
  def self.handle options, ex
    return if ! ex['boostrap'] || options[:output]

    txt = File.read "#{options[:last_source_dir]}#{ex['boostrap']}"

    if options[:client] == :web
      txt = Bootstrap.render txt
    end

    options[:output] = txt
  end
end
