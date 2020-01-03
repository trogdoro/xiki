module Xiki
  class Accounts
    def self.menu *args
      Notes.drill '%accounts', *args
    end
  end
end
