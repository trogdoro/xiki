class AddressBook
  def self.menu key=nil
    if key.nil?
      `osascript -e 'launch app "Address Book"'`
      names = `osascript -e 'tell app "Address Book" to get the name of every person'`
      return names.split(', ').sort.uniq.map{|o| "#{o}/"}
    end

    '| address info'
  end
end
