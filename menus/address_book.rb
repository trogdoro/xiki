class AddressBook
  def self.names
    `osascript -e 'launch app "Address Book"'`
    names = `osascript -e 'tell app "Address Book" to get the name of every person'`
    names.split(', ').sort.uniq
  end

  def self.menu key=nil
    return self.names.map{|o| "#{o}/"} if key.nil?

    '| todo!'
  end

end
