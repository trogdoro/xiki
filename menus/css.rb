class Css
  def self.menu_before *args
    return if args[0] != "list"

    args.shift

    # If just "css/list/", list all

    if args.blank?
      js = %`
        var result="";
        $('*[class]').each(function(i, e){
          var id = $(e).attr('class');
          result += "+ "+id+"/\\n";
        })
        result;
        `.unindent

      result = Firefox.run js
      return "> No class attributes found!" if result == '""'
      return result
    end

    # Add "." to beginning of first (it was removed due to it meaning a method call)
    args.first.sub! /^/, '.'

    Dom.dom args
  end

  def self.menu *args

    if args.empty?
      return "
        + list/
        | /* Or type some css here (to run in the browser) */
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
