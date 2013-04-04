require 'rr'

# RSpec::Runner.configure do |config|
RSpec.configure do |config|
  config.mock_with :rr
end


module Xiki
  def self.dir
    File.expand_path("#{File.dirname(__FILE__)}/..") + "/"
  end
end

def stub_menu_path_env_dirs
  xiki_dir = Xiki.dir

  list = ["#{xiki_dir}spec/fixtures/menu", "#{xiki_dir}menu2"]
  stub(Menu).menu_path_env_dirs {list}
end
