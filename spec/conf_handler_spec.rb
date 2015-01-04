$:.unshift "spec/"

require './spec/spec_helper'

require 'xiki/handlers/conf_handler'

%w"xiki/core/tree".each {|o| require o}

# No longer used - Xik.new now is used instead

