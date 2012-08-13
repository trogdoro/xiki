# Generated by jeweler
# DO NOT EDIT THIS FILE DIRECTLY
# Instead, edit Jeweler::Tasks in Rakefile, and run 'rake gemspec'
# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = "xiki"
  s.version = "0.5.0a"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Craig Muth"]
  s.date = "2011-08-24"
  s.executables = ["xiki"]
  s.summary = "A shell console with GUI features."
  s.description = "Xiki does what shell consoles do, but lets you edit everything at any time. It's trivial to make your own commands and menus to access other tools."
  s.email = "craig.muth@gmail.com"
  s.extra_rdoc_files = [
    "LICENSE",
    "README.markdown"
  ]
  files = `git ls-files`.split("\n")

# TMP
  files = files.select{|o| o !~ /^etc\/xiki/}

  s.files = files
  s.homepage = "http://xiki.org"
  s.licenses = ["MIT"]
  s.require_paths = ["."]
  #   s.rubygems_version = "1.8.24"

  #     current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
  #     s.specification_version = 3

  s.add_development_dependency "rspec"
  s.add_dependency 'ruby2ruby'
  s.add_dependency 'ParseTree'
  s.add_dependency 'httparty'
  s.add_dependency 'activesupport'
  s.add_dependency 'method_source'
  s.add_dependency 'net-ssh'
  s.add_dependency 'net-sftp'
  s.add_dependency 'memcached'
  s.add_dependency 'el4r'
  s.add_dependency 'rake'
  s.add_dependency 'daemons'


end
