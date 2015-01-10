# -*- encoding: utf-8 -*-


# Xiki is temporarily not set up to be used as a gem.  Just download it
# and run bin/xsh directly.
#
# See comment in Gemfile for more details


#Gem::Specification.new do |s|
#  s.name = "xiki"
#  s.version = "2.0.1a"
#
#  s.required_ruby_version = ">= 1.9.3"
#
#  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
#  s.authors = ["Craig Muth"]
#  s.date = "2014-09-28"
#
#"
#/projects/xiki/
#  - README.md
#    : - wait, maybe just move loop into different file?
#    :   - if I don't have Gemfile:_gemspec, it probably won't do the binary?
#    :     - try just hard-coding the Gemfile with ''gem 'foo' files''
#"
#
## If not, this should fix it
##
##- Maybe instructions should be for doing "bundle install" in subdir?
##  - So, making the directions be
##    | $ cd $xiki_dir/install
##    | $ bundle install
##    - look at what they are now!
##  - and move dependencies out into a 3rd file?
##    - loop through and do "s.add_dependency" or "gem" from Gemfile and .gemspec?
#
#
#  # Check out what the methods on "s" are
#    # It might clue in whether running from bundler
#
#  # Try removing s.executables...
#    # to see if that'll still cause it to add it to the path
#      # when "$ bundle install"
#
#  if ENV['_'] !~ /\/bundle$/   # If not called by bundler, install executables
#    s.executables = ["xiki"]
#  end
#
#
#
#
#
#
#  s.summary = "A shell console with GUI features."
#  s.description = "Xiki does what shell consoles do, but lets you edit everything at any time. It's trivial to make your own commands and menus to access other tools."
#  s.email = "craig.muth@gmail.com"
#  s.extra_rdoc_files = [
#    "LICENSE",
#    "README.md"
#  ]
#
#  s.rdoc_options += %w[--exclude misc/templates/.*]
#
#  files = `git ls-files`.split("\n")
#  files = files.select{|o| o !~ /^etc\/xiki/}
#  s.files = files
#
#  s.homepage = "http://xiki.org"
#  s.licenses = ["MIT"]
#  s.require_paths = ["."]
#  s.require_paths = ["lib"]
#
#  #   s.add_dependency('awesome_print')
#  #   s.add_dependency('json')
#  #   s.add_dependency('httparty')
#  #   s.add_dependency('activesupport')
##  s.add_dependency('erubis')
#  #s.add_dependency('method_source')
#  #s.add_dependency('net-ssh')
#  #s.add_dependency('net-scp')
#  #s.add_dependency('net-sftp')
#
#  #   s.add_dependency('rake')
#  #   s.add_dependency('rspec', [" ~> 2.12.0"])
#  #   s.add_dependency('trogdoro-el4r', [">= 1.0.10"])
#
##  s.add_dependency('sexp_processor')
#  #   s.add_dependency('file-tail')
#  #   s.add_dependency('ruby_parser')
#  #   s.add_dependency('sourcify')
#  #   s.add_dependency('daemons')
#  #   s.add_dependency('simple-tidy')
#  #   s.add_dependency('nokogiri-pretty')
#  #   s.add_dependency('sinatra')
#  #   s.add_dependency('session')   # For keeping a bash session open to send shell commands to
#  # s.add_dependency('map')   # For keeping a bash session open to send shell commands to
#
#  # Restore if changes get merged into main el4r gem
#  #  s.add_dependency('el4r')
#
#
#
#
#
#  # TODO > put instructions here "like 'run the xiki command in the shell console"
#  s.post_install_message = "Thanks for installing Xiki.\n\nInstructions\n\nRun the 'xiki' command in a shell console.  It will walk you through getting started."
#
#  # Make the 'xiki' command say to start the web server?
#
#end
