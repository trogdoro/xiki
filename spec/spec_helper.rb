require 'rr'
require 'awesome_print'

XIKI_SERVER_MODE = false if ! defined?(XIKI_SERVER_MODE)

%w"xiki/core/core_ext xiki/core/ol".each {|o| require o}

# RSpec::Runner.configure do |config|
RSpec.configure do |config|
  config.mock_with :rr
  config.expect_with(:rspec) { |c| c.syntax = :should }
end

module Xiki
  def self.dir
    File.expand_path("#{File.dirname(__FILE__)}/..") + "/"
  end
end

def stub_menu_path_dirs
  xiki_dir = Xiki.dir

  list = ["#{xiki_dir}spec/fixtures/menu", "#{xiki_dir}commands"]
  stub(Xiki).xiki_path_dirs {list}
end


# Put this here until we can put it in a better place
#  - Maybe so it'll be used by main xiki and conf
AwesomePrint.defaults = {
  :indent => -2,   # left-align hash keys
  :index => false,
  :multiline => true,
}

include Xiki   # Allows all specs to use classes in the Xiki module without "Xiki::"
