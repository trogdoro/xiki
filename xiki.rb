XIKI_ROOT = File.dirname(__FILE__)
Dir.chdir(XIKI_ROOT)

# Require some of the core files
require 'rubygems'
require 'ol'
require 'requirer'
require 'text_util'
require 'notes'

# Get rest of files to require
classes = Dir["**/*.rb"]
classes = classes.select{|i| i !~ /xiki.rb$/}   # Remove self
classes = classes.select{|i| i !~ /key_bindings.rb$/}   # Remove key_bindings
classes = classes.select{|i| i !~ /tests\//}   # Remove tests

classes.map!{|i| i.sub(/\.rb$/, '')}

# Require classes them
Requirer.safe_require classes

# key_bindings has many dependencies, require it last
Requirer.safe_require ['key_bindings.rb']
