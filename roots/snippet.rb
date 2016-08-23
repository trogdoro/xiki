module Menu
  class Snippet
    def self.menu *args

      if args[0] == "docs"
        return Tree.children self.docs, args[1..-1]
      end

      extension = View.extension
      file = "#{Xiki.dir}misc/snippets/#{extension}.notes"

      prefix = Keys.prefix

      return View.open file if prefix == 'open' || prefix == :u   # If up+, just go to file

      # If leaf has quote, kill the rest or create new snippet

      if args[-1] =~ /\|/
        txt = Tree.txt

        txt << "\n" if txt !~ /\n/

        Tree.to_root
        CodeTree.kill_rest
        indent = Line.indent
        Line.delete
        View.<< txt.gsub(/^/, indent), :dont_move=>1

        return
      end

      # Get snippets.foo file from snippets/ dirs, and drill into it

      # TODO: also look in ~/xiki/snippets? - find different dir name, in case ~/xiki is the Xiki.dir

      if ! File.exists? file
        # TODO: if none found, show
        return "
          | No snippets were found for the file extension '#{extension}'.
          | You can create some by making a tree in this file:
          @ #{file}
          "
        # TODO: maybe let them type directly into the @snippet menu and do C-. to save
      end

      txt = File.read file
      Tree.children txt, args

      # TODO: if line has quote and path doesn't exist in file (.drill fails), create new snippet

    end
  end

  #   def self.docs
  #     "
  #     > To edit

  #     > To Use
  #     ___
  #     "
  #   end

end
