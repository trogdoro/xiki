class AddressBook
  def self.names
    names = Applescript.run "address book", "get the name of every person"
    names.sub! /^\{(.+)\}/, "[\\1]"
    JSON[names]
  end

  def self.menu key=nil
    return self.names.map{|o| "#{o}/"} if key.nil?

    # Clean up a bit
    txt = Applescript.run "address book", "get vcard of people whose name is \"#{key}\""
    txt.gsub! /, /, "\n"
    txt.gsub! /:/, ": "
    txt = txt.split("\n").select{|o| o !~ /^"}|missing value|END: VCARD|VERSION: 3.0|BEGIN: VCARD/}.join("\n")
    txt.gsub! /^/, "- "

    txt
  end

end
