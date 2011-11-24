class Foo
  def self.menu
    "
    - sammiches/
      - ham/
        - .buy/
      - tofu/
        - .buy/
    - .checkout/
      - cash/
      - credit/
    "
  end
  def self.buy category, item
    "- buying #{item} #{category}"
  end

  def self.checkout kind
    "- checking out as #{kind}..."
  end

  def self.admin
    "
    - reports/
      - .profit/
      - .loss/
    - .restart_server/
    "
  end
end
