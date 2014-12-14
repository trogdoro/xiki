module Xiki
  class Quick

    MENU = "
      =conf/
      - .docs/
        - keys/
          - labels in tasks.notes/
            > do+quick
            | Grabs labelled lines from tasks.notes.
          - shortcuts in =conf/
            > as+quick
            | Saves current file in the =quick menu.
            > open+quick
            | Lists the current items, so you can quickly jump to them.
          - shortcuts in bookmarks/
            > layout+quick
            | Reads shortcuts from a bookmark.
      "

    MENU_HIDDEN = "
      .save
      "

    # =commit/do+quick: created

    def self.save

      options = yield

      target_file = options[:ancestors][-1].sub /\/$/, ''

      conf_file = File.expand_path("~/xiki/commands/conf/quick.conf")
      txt = File.read conf_file

      # If a file, use View.open...
      if File.exists? target_file
        key = target_file[/.+\/(\w+)/, 1]
        target_code = "View.open '#{target_file}'"
      else   # If a buffer, use Buffers.to...
        key = target_file
        target_code = "Buffers.to '#{target_file}'"
      end

      txt.sub! /^-/, "- #{key}/\n  ! #{target_code}\n-"

      File.open(conf_file, "w") { |f| f << txt }

      View.message "saved to =quick menu"
      View.kill if View.name == "quick/save/"
    end

    def self.menu_after output, *path

      # Don't do anything if path exists and output - already handled...

      return if output && path.any?

      # If there's a $bookmark, grab from it
      return self.menu_after_with_bookmark path if path[0] =~ /^\$/
      options = yield

      conf = options[:conf]

      txt = Tree.children conf, path

      View.kill :force_recent=>1 if View.name == "quick/" && txt =~ /\A!/

      MenuHandler.eval_exclamations txt

      return "" if ! output

      # /, so add =conf at top
      if path.blank?
        # Add the items after =conf
        output.sub!(/=conf\/\n/, "\\0#{txt}")
      end

      output
    end


    def self.menu_after_with_bookmark path
      prefix = Keys.prefix :clear=>1

      bookmark = path.slice!(0)#.sub(/\$/, '')
      bookmark = Bookmarks[bookmark]

      raise "- Bookmark not found!" if bookmark =~ /\$/

      # /$foo, so show labels...

      if path.empty?
        txt = File.read bookmark, :encoding=>'binary'
        txt = txt.scan(/^ *[+-] ([^(\n]+?)\)/).flatten
        txt.uniq!
        txt = txt.map{|o| "+ #{o}/\n"}.join
        return txt
      end

      # /$foo/label, so jump to label and run...

      View.kill if View.name =~ /^=quick\//

      View.open bookmark
      View.to_highest
      found = Search.forward "^ *[+-] #{Emacs.regexp_quote path[0]})"
      return "<! '#{path[0]}' not found!" if ! found

      # Just navigate if as+open
      return Move.forward if prefix == "open"

      # Move down one if just a label
      Move.forward if Line.without_label.blank?

      Tree.kill_under
      Launcher.launch

      nil
    end

  end
end
