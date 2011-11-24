class AddressBook
  def self.menu
    `osascript -e 'launch app "Address Book"'`
    names = `osascript -e 'tell app "Address Book" to get the name of every person'`
    names = names.split(', ').sort.uniq.map{|o| "#{o}/"}
  end
end
