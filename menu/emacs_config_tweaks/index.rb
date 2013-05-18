class EmacsConfigTweaks
  def self.menu_after output, item=nil, content=nil

    return if ! content   # Do nothing unless content

    # /foo/|(content, so add it to .emacs...

    content = Tree.txt

    txt = File.read(File.expand_path("~/.emacs"), *Files.encoding_binary)

    # If already in .emacs, just say so...

    snippet_to_search_for = content.dup
    snippet_to_search_for.sub!(/\A;.+\n/, '')   # Ignore comment
    if txt =~ /#{Regexp.escape snippet_to_search_for}/
      return "- It appears part of this is already in your .emacs!"
    end

    # Add to the end of .emacs...

    txt.strip!
    txt << "\n\n#{content}"

    File.open(File.expand_path("~/.emacs"), "w") { |f| f << txt }
    "@flash/- added this to your .emacs!"
  end
end
