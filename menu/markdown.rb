gem 'redcarpet'
require 'redcarpet'

class Markdown

  def self.render txt
    markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML, :autolink=>true, :space_after_headers=>true)
    html = markdown.render txt
    html << "
      <style type='text/css'>
        body {
        font-family: arial;
        font-size: 12px;
        }
        pre {
          background-color: #F8F8F8;
          border: 1px solid #CCCCCC;
          border-radius: 3px 3px 3px 3px;
          font-size: 13px;
          line-height: 19px;
          overflow: auto;
          padding: 6px 10px;
        }
      </style>
      ".unindent

    html
  end

  def self.menu *args

    # If nothing passed, show example markdown
    return "
      > Render the markdown wiki format in the browser
      | # Heading
      | ## Small Heading
      |
      |     Code is indented
      |     four or more spaces.
      |
      | - Bullet
      |    - Another (indented 3 spaces each)
      |
      | A normal sentence.
      " if args.blank?

    html = self.render(ENV['txt'])

    Browser.html html

  end
end
