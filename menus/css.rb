class Css
  def self.menu *args

    return "| h1 {color:red;}  /* Type some css here (to run in the browser) */" if args.empty?

    txt = ENV['txt']

    txt.gsub!("\n", '\n')
    txt.gsub!('"', '\"')
    code = "$('head').append(\"<style>#{txt}</style>\")"
    Firefox.run code  #, :jquery=>1

    nil
  end
end
