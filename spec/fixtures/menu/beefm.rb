class Beefm
  def self.menu *args
    "
    - moo/
    "
  end
  def self.menu_before *args
    "lika tolya befo: #{args.inspect}"
    nil
  end
end
