module Xiki
  class Sammiches
    def self.menu
      "
      - meat/
        - ham/
          - .buy/
        - philly/
          - .buy/
      - veggie/
        - cucumber/
          - .buy/
        - bark/
          - .buy/
      - .checkout/
        - cash/
        - credit/
      "
    end
    def self.buy category, item
      "- adding to cart #{item} #{category}"
    end

    def self.checkout kind
      "- checking out as #{kind}..."
    end

  end
end
