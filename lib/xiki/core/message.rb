module Xiki
  class Message
    def self.<< txt
      View.message txt
    end
  end
end
