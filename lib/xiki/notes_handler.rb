class NotesHandler
  def self.handle options, ex
    return if ! ex['notes'] || options[:output] || options[:halt]
    path = "#{options[:last_source_dir]}#{ex['notes']}"

    txt = Notes.drill path, *options[:args]||[]

    options[:output] = txt
    options[:halt] = 1   # Just in case there's no output
  end
end
