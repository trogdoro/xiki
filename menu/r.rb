gem 'httparty'; require 'httparty'

#
# Runs code in a rails app.  Implants xiki_dev_controller.rb into the
# app, so it can pass messages to it to eval.
#
# For security it only works in dev mode, only accepts local requests,
# and reads input from a file on disk.
#
class R
  def self.menu code=nil, yaml=nil

    # If nothing passed, show last r/... commands...

    if ! code
      txt = Launcher.last "r", :exclude_path=>1
      txt.gsub! /^- (\| )?/, '| '
      txt = txt.split("\n").uniq.join("\n")
      return txt
    end

    ENV['no_slash'] = "1"

    if yaml
      txt = ENV['txt']
      txt = Rails.run_in_app txt, :yaml=>1
    else
      txt = Rails.run_in_app ENV['txt']
      txt = Tree.quote(txt) if txt !~ /^\s+>/
    end

    txt
  end

  def self.suggest_creating_controller
    %`
    | This rails app may not have the xiki dev controller
    | installed to let xiki evaluate code in it in dev mode.
    | Create it?
    @#{Projects.default}
      - app/controllers/
        - xiki_dev_controller.rb
          | class XikiDevController < ApplicationController
          |   def index
          |     return render(:text=>"Disabled unless development and called locally.") if ! Rails.env.development? || request.remote_ip != "127.0.0.1"
          |     code = File.read "/tmp/rails_run_tmp.txt"
          |     txt = eval code
          |     render :text=>txt.to_yaml
          |   end
          | end
      - config/routes.rb
        |+  match 'xiki_dev' => 'xiki_dev#index'
        | end
    `
  end

end
