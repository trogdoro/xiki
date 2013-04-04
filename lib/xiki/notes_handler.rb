class NotesHandler
  def self.handle options, ex
    return if ! ex['notes'] || options[:output]

    path = "#{options[:last_source_dir]}#{ex['notes']}"

    txt = Notes.drill path, *options[:args]||[]
    options[:output] = txt || :none
  end
end
