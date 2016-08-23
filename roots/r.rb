# Require 'rails' menu if not yet loaded
load "#{Xiki.dir}commands/rails.rb" if ! defined?(Xiki::Rails)

module Xiki

  # Runs code in a rails app.  Implants xikidev_controller.rb into the
  # app, so it can pass messages to it to eval.
  #
  # For security it only works in dev mode, only accepts local requests,
  # and reads input from a file on disk.
  class R

    def self.menu *args
      code, yaml = args

      return "=beg/quoted/" if args[-1] && args[-1] !~ /\n/

      # /, so show last r/... commands...

      if ! code
        Ol["This should get just r/, not all r... menu roots!"]
        txt = Launcher.last "r", :exclude_path=>1
        txt.gsub! /^- (\| )?/, '| '
        txt = txt.split("\n").uniq.join("\n")
        return txt
      end

      #       ENV['no_slash'] = "1"


      # /code/, so run it in app...

      if ! yaml
        txt = Rails.run_in_app code
        txt = Tree.quote(txt) if txt !~ /^\s+(>|\|)/
        return txt
      end

      # /code/yaml, so save the yaml (assuming it's an active record object)...
Ol "yaml", yaml

      Rails.run_in_app yaml, :yaml=>1
    end

  end
end
