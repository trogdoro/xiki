class Ri
  def self.menu
    %`
      > Type class or method here.  Examples...
      | String
      | File.new
      | zip
      | Array.[]
      | compact!
      - .list/
      - docs/
        - summary/
          | Wraps the "ri" command, to show documentation for ruby.  Type
          | ruby classes or methods etc, to see their "ri" documentation.
          | Try these out:

          @ri/String
          @ri/to_s

          | Tip: use search+like+menu+"ri" to lookup docs for a string in
          | a file.
        - ri command help/
          @$ ri --help
      - build index first/
        @% rvm docs generate-ri
      `
  end

  def self.menu_after output, *args
    return if output

    # .menu didn't handle, so pass to "ri" command...

    # Make it in the form of Class.foo
    txt = args.select{|o| o !~ /^\./}.map{|o| o.sub(/^\|/, '').strip}.join "."

    txt = `ri --format=rdoc #{txt}`

    txt = Tree.quote txt

    # Massage it to have formatted headings, etc.
    txt.gsub! /^\| \(from ruby site\)\n/, ""
    txt.gsub! "| === Implementation from ", "- Implementation from: "
    txt.gsub! /^\| = (.+)/, "> \\1"
    txt.gsub! /^\| === /, "> "
    txt.gsub! /^\| ---+\n(\|\n)?/, ""

    txt
  end

  def self.list *args
    return nil if args.any?   # /list/| String, output nil so .menu_after handles it

    Tree.quote `ri --list`
  end
end
