module Xiki
  class XikiHandler
    def self.handle options

      # Look at source file to see if it has >... headings...

      source_file = options[:handlers]['xiki']
      return if ! source_file

      file = "#{options[:enclosing_source_dir]}#{source_file}"
      txt = File.read file, *Xiki::Files.encoding_binary
      # Probably save this into options, so we don't have to read it again in one of the other handlers (is there a convention for a key name for this?)

      txt.gsub! "\r\n", "\n"   # In case dos linebreaks
      headings = txt.scan(/^>/)

      # >... at top and thereafter, so delegate to NotesHandler...

      return NotesHandler.handle(options) if headings.length > 1 && txt =~ /\A>/

      # No >..., so delegate to MenuHandler...

      MenuHandler.handle options

    end

  end
end
