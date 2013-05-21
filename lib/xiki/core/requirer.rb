require 'xiki/core/core_ext'

class Requirer
  def self.show txt
    if ! $el
      return if txt !~ /(^Xiki requires|exception:$)/
      return puts "#{txt}"
    end

    $el.switch_to_buffer "Issues Loading Xiki"
    $el.insert txt
    $el.bury_buffer unless $el.buffer_substring($el.point_min, $el.point_max) =~ /(^Xiki requires|exception:$)/
  end

  def self.require_gem name, options={}
    begin
      gem name
      require options[:name2] || name
    rescue Exception=>e
      self.show "Xiki #{options[:optional] ? 'optionally uses' : 'requires'} the '#{name}' gem.\n% gem install #{name}\n\n"
      raise e if options[:raise]
    end
  end

  def self.extract_gem_from_exception txt
    txt = txt[/-- (.*)/, 1] || txt[/RubyGem (.*)/, 1] || txt[/Could not find (.+?) /, 1] || txt

    return nil if ! txt

    txt.sub!(/\(.+/, '')
    txt.strip
  end

  def self.require_classes files
    files.each do |l|
      begin
        require l
      rescue LoadError => e
        self.show "#{e.to_s}\n"
        gem_name = self.extract_gem_from_exception e.to_s
        self.show %`Xiki requires the '#{gem_name}' gem.  Have you run "bundle install"?\n% gem install #{gem_name}\n\n`
      end
    end

  end
end
