class NotesHandler
  def self.handle options
    source = options[:ex]['notes']

    return if ! options[:ex] || options[:output] || options[:halt]
    path = "#{options[:last_source_dir]}#{source}"

    txt = Notes.drill path, *options[:args]||[]

    options[:output] = txt
    options[:halt] = 1   # Just in case there's no output
  end
end
