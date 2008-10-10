require 'notes'

class Requirer
  def self.safe_require files

    files.each do |l|
      begin
        require l
      rescue LoadError => e

        View.to_buffer '* missing gems'
        Notes.mode
        gem_name = e.to_s[/-- (.*)/, 1] || e.to_s[/RubyGem (.*)/, 1] || gem_name
        gem_name.sub!(/\(.+/, '').strip!
        View.insert TextUtil.unindent("
          | Warning: gem '#{gem_name}' missing
          - Explanation:
            A gem used by a Xiki class is not installed on your system.
            Note that installing it isn't mandatory.
            - Missing gem: #{gem_name}
            - Class that uses it: #{l}

          - To install '#{gem_name}':
            - 1: Double-click or C-. on this line
              !!sudo gem install #{gem_name}
            - 2: Press M-l to reload xiki, or manually restart.
          \n
          ")
        View.to_top
      end
    end

  end
end
