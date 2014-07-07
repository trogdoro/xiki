# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = "xiki"
  s.version = "1.0.1a"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Craig Muth"]
  s.date = "2013-05-20"

  if ENV['_'] !~ /\/bundle$/   # If not called by bundler, install executables
    s.executables = ["xiki"]
  end

  s.summary = "A shell console with GUI features."
  s.description = "Xiki does what shell consoles do, but lets you edit everything at any time. It's trivial to make your own commands and menus to access other tools."
  s.email = "craig.muth@gmail.com"
  s.extra_rdoc_files = [
    "LICENSE",
    "README.md"
  ]

  s.rdoc_options += %w[--exclude etc/templates/.*]

  files = `git ls-files`.split("\n")
  files = files.select{|o| o !~ /^etc\/xiki/}
  s.files = files

  s.homepage = "http://xiki.org"
  s.licenses = ["MIT"]
  s.require_paths = ["."]
  s.require_paths = ["lib"]

  s.add_dependency('awesome_print')
  s.add_dependency('json')
  s.add_dependency('httparty')
  s.add_dependency('activesupport')
  s.add_dependency('erubis')
  s.add_dependency('method_source')
  s.add_dependency('net-ssh')
  s.add_dependency('net-scp')
  s.add_dependency('net-sftp')
  s.add_dependency('rake')
  s.add_dependency('rspec', [" ~> 2.12.0"])
  s.add_dependency('trogdoro-el4r', [">= 1.0.10"])

  s.add_dependency('sexp_processor')
  s.add_dependency('file-tail')
  s.add_dependency('ruby_parser')
  s.add_dependency('sourcify')
  s.add_dependency('daemons')
  s.add_dependency('simple-tidy')
  #   s.add_dependency('nokogiri-pretty')
  s.add_dependency('sinatra')

  s.add_dependency('rr')

  # Restore if changes get merged into main el4r gem
  #  s.add_dependency('el4r')
end
