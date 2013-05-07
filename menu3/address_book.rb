class AddressBook
  def self.names
    names = Applescript.run "get the name of every person as string", :app=>"address book", :delimiter=>"|"
    names.split "|"
  end

  def self.menu key=nil
    return self.names.map{|o| "#{o}/"} if key.nil?

    # Clean up a bit
    # Broken by crap implementation of (do-applescript in emacs 23 that returns fucking "true" instead of the data)
      # Emacs 22 implementation returned data structure as text - is there a way to force that in applescript?
    txt = Applescript.run "get vcard of people whose name is \"#{key}\"", :delimiter=>"|", :app=>"address book"

    txt.gsub! /, /, "\n"
    txt.gsub! /:/, ": "
    txt = txt.split("\n").select{|o| o !~ /^"\}|missing value|END: VCARD|VERSION: 3.0|BEGIN: VCARD/}.join("\n")
    txt.gsub! /^/, "- "

    txt
  end

end
