class Json
  def self.extract_json_header txt

    header = ""

    # "{{" at beginning, so remove 1st and return no header...

    if txt =~ /\A\{\{/
      txt.sub!(/./, '')
      return nil   # No header exists
    end

    # No "{" at beginning, so return no header
    if txt !~ /\A\{/
      return nil
    end

    # Single json line at top, so extract it as header
    if txt =~ /\A.+\}$/
      return txt.slice! /.+$\n?/
    end

    # Multi-line json header, so extract until 1st "}" on a line by itself
    txt.slice!(/.+?\n}$\n?/m)


    # return header == "" ? nil : header
  end
end
