module Xiki
  class Encoding
    MENU = "
      - .dos/
      - .raw unix/
      - .iso unix/
      - .utf8/
      - see/
        <@ specials/
      "

    def self.menu_after out, *args
      return if args.any?
      "- current: #{$el.elvar.buffer_file_coding_system.to_s}\n#{out}"
    end

    def self.dos
      $el.set_buffer_file_coding_system :dos
      "<* updated!"
    end

    def self.raw_unix
      $el.set_buffer_file_coding_system :unix
      "<* updated!"
    end

    def self.iso_unix
      $el.set_buffer_file_coding_system :iso_latin_1_unix
      "<* updated!"
    end

    def self.utf8
      $el.set_buffer_file_coding_system :mule_utf_8_unix
      "<* updated!"
    end

  end
end
