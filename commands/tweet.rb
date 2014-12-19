require "#{Xiki.dir}commands/tweets"

module Menu
  class Tweet

    MENU = %`
      `
    def self.menu_after output, txt=nil, *extra

      options = yield

Ol.a options

      # Dropdown...

      return "~ show in browser" if options[:dropdown] == []
Ol "options[:dropdown]", options[:dropdown]   # => ["show in browser"]
Ol "txt", txt
      if (options[:dropdown] == ["show in browser"] || options[:prefix] == "open") && (!txt || txt =~ /\n/ || txt =~ /^\|/)   # as+open, so just open my profile
        Browser.url "https://twitter.com/#{Tweets.active_account}", :activate=>1
        return ""
      end

      # /, so show sample tweet...

      if ! txt

        #         active = accounts[/(.+)\n.+ \(active\)/, 1]
        active = Tweets.active_account
        return "
          > Tweet as @#{active}
          | An example tweet!
          ".unindent # +output
        Ol["Make right-click have > ~ tweets/"]
        # << tweets/
      end

      # /something from MENU, so don't interfere...

      return if output

      # /123123, so show tweet for this id...

      if txt =~ /\A\d+\z/

        # as+open, so open url...

        return Browser.url "https://twitter.com/x/status/#{txt}/" if options[:prefix] == "open"

        # Send tweet...

        result = Shell.sync "t status #{txt}"
        result.gsub! /^(Text|Screen name)/, "| \\1"
        result.gsub! /^\w.+\n/, ''
        #         return Tree
        return result
      end

      # /| tweet, so send it...

      # If not at least 2 spaces, it was probably a menu
      return "- Doesn't look like a valid tweet" if txt !~ / .+ / || extra.any?

      txt.strip!

      return "- > 140 characters (#{txt.length})!" if txt.length > 140

      txt.gsub!("'", "'\\\\''")

      command = "t update '#{txt}'"
      result = Shell.sync command
Ol "result", result
      #       return "> Error\n#{Tree.quote result, :char=>'|'}" if result =~ /\AERROR:/
      return "> Error\n#{Tree.quote result, :char=>'|'}" if result =~ /[\s>]*error/i

      "<! tweeted!"

    end

  end
end

