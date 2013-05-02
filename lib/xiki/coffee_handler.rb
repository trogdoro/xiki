class CoffeeHandler
  def self.handle options, ex
    return if ! ex['coffee'] || options[:output] || options[:halt]

    source = "#{options[:last_source_dir]}#{ex['coffee']}"
    txt = Console.sync "coffee -pc \"#{source}\" | js"

    options[:output] = txt
  end
end
