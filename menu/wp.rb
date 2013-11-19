load "#{Xiki.dir}menu/wikipedia.rb" if ! defined?(Wikipedia)
Wikipedia.wp Tree.rootless(options[:path]), options
