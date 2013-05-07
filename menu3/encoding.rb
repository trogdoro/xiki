class Encoding
  def self.menu
    "
    - current: #{$el.elvar.buffer_file_coding_system.to_s}
    - .dos/
    - .raw unix/
    - .iso unix/
    - .utf8/
    - see/
      <@ specials/
    "
  end

  def self.dos
    $el.set_buffer_file_coding_system :dos
    ".flash - updated!"
  end

  def self.raw_unix
    $el.set_buffer_file_coding_system :unix
    ".flash - updated!"
  end

  def self.iso_unix
    $el.set_buffer_file_coding_system :iso_latin_1_unix
    ".flash - updated!"
  end

  def self.utf8
    $el.set_buffer_file_coding_system :mule_utf_8_unix
    ".flash - updated!"
  end

end
