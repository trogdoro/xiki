class Css
  def self.menu *args

    if args.empty?
      return "
        | /* Type some css here (to run in the browser) */
        | h1 { color: red; }
        | body {
        |   font-family: arial;
        |   font-size: 12px;
        |   color: #000;
        |   background-color: #fff;
        | }
        "
    end

    txt = ENV['txt'].dup

    txt.gsub!("\n", '\n')
    txt.gsub!('"', '\"')
    code = "$('head').append(\"<style>#{txt}</style>\")"
    Firefox.run code  #, :jquery=>1

    nil
  end
end
