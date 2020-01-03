require "#{Xiki.dir}roots/dom"

module Xiki
  class Css
    def self.menu_before *args

      # If css/list...

      return self.list args[1..-1] if args[0] == "list"

      # Make this start with dot now?
      # If css/foo, treat as style

      if args[0] && args[0] =~ /^[a-z]/i && args[0] !~ /\n/
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

        result = Browser.js js

        # When dups, make foo,foo be foo,foo:2, etc.
        result = self.add_nth_to_dups result

        return "> No class attributes found in page!" if result == '""'
        return result.gsub /^\+ /, '+ .'
      end

      # Add "." to beginning of first (it was removed due to it meaning a method call)
      #       args.first.sub! /^/, '.'

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
          |   background-color: #eee;
          |   margin: 30px;
          | }
          "
      end

      txt = args[0]
      return "=beg/quoted/" if txt !~ /\n/

      # =extract

      self.send_to_browser txt

      nil
    end

    def self.send_to_browser txt
      txt.gsub!("\n", '\n')
      txt.gsub!('"', '\"')
      code = "$('head').append(\"<style>#{txt}</style>\")"
      # Firefox.exec code
      Browser.js code
      nil
    end


    # When dups, make foo,foo be foo,foo:2 etc.
    def self.add_nth_to_dups txt

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
end
