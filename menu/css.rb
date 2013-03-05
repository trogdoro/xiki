class Css
  def self.menu_before *args

    # If css/list...

    return self.list args[1..-1] if args[0] == "list"

    # If css/foo, treat as style

    if args[0] && args[0] =~ /^[a-z]/i
      return self.list args
    end


    nil
  end

  def self.list args

    # If just "css/list/", list all...

    if args.blank?
      js = %`
        var result="";
        $('*[class]').each(function(i, e){
          var id = $(e).attr('class');
          result += "+ "+id+"/\\n";
        })
        result;
        `.unindent

      result = Firefox.exec js

      # When dups, make foo,foo be foo,foo:2, etc.
      result = self.add_nth_to_dups result

      return "> No class attributes found!" if result == '""'
      return result
    end

    # Add "." to beginning of first (it was removed due to it meaning a method call)
    args.first.sub! /^/, '.'

    Dom.dom *args
  end

  def self.menu *args

    if args.empty?
      return "
        | /* Or type some css here (to run in the browser) */
        | h1 { color: red; }
        | body {
        |   font-family: arial;
        |   font-size: 15px;
        |   color: #333;
        |   background-color: #fff;
        |   margin: 30px;
        | }
        + list/
        "
    end

    txt = ENV['txt'].dup

    txt.gsub!("\n", '\n')
    txt.gsub!('"', '\"')
    code = "$('head').append(\"<style>#{txt}</style>\")"
    Firefox.exec code

    nil
  end

  # When dups, make foo,foo be foo,foo:2 etc.
  def self.add_nth_to_dups txt

    # Why hard-coded?
    #     txt = "+ navbar navbar-inverse navbar-fixed-top/\n+ navbar-inner/\n+ container/\n+ container/\n+ hero-unit/\n"

    dups = {}
    txt = txt.split("\n")
    txt.each do |line|
      dups[line] ||= 0
      dups[line] += 1
      line.sub! "/", ":#{dups[line]}/" if dups[line] > 1
    end

    txt.join("\n")

  end

end
