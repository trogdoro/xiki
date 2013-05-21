class CoffeeHandler
  def self.handle options
    source = options[:ex]['coffee']
    return if ! source || options[:output] || options[:halt]

    source = "#{options[:last_source_dir]}#{source}"
    txt = Console.sync "coffee -pc \"#{source}\" | js"

    options[:output] = txt
  end
end
