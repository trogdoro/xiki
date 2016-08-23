gem 'redcarpet'
require 'redcarpet'

class Markdown
  # include Xiki

  def self.render txt
    require "#{Xiki.dir}commands/html"

    markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML, :autolink=>true, :space_after_headers=>true)
    html = markdown.render txt

    html << Xiki::Html.default_css

    html
  end

  def self.menu_before *path
    # If just @path/mode, just enable mode
    if path == ["mode"]
      self.define_styles
      return
    end

    nil
  end

  def self.define_styles
    Styles.apply("^\\(# \\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
    Styles.apply("^\\(## \\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)
    Styles.apply("^\\(### \\)\\(.*\n\\)", nil, :notes_h3_pipe, :notes_h3)
    Styles.apply("^\\(#### \\)\\(.*\n\\)", nil, :notes_h4_pipe, :notes_h4)
  end

  def self.menu txt=nil

    # If nothing passed, show example markdown
    return "
      > Render the markdown wiki format in the browser
      : # Heading
      : ## Small Heading
      :
      :     Code is indented
      :     four or more spaces.
      :
      : - Bullet
      :    - Another (indented 3 spaces each)
      :
      : A normal sentence.
      " if ! txt

    html = self.render txt

    Browser.html html

  end
end
