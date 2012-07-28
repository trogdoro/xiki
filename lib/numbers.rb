class Numbers
  def self.menu
    ['.sum_numbers_in_clipboard',
      '.sum_dollars_in_clipboard'
    ]
  end

  def self.sum_numbers txt
    numbers = txt.scan(/[\d.]+/)
    numbers.map!{|o| o.to_f}
    numbers.inject(0){|i, o| i + o}
  end

  def self.sum_numbers_in_clipboard
    self.sum_numbers Clipboard[0]
  end

  def self.sum_dollars txt
    numbers = txt.scan(/\$([\d.]+)/).map{|o| o.first}
    numbers.map!{|o| o.to_f}
    numbers.inject(0){|i, o| i + o}
  end

  def self.sum_dollars_in_clipboard
    self.sum_dollars Clipboard[0]
  end

  def self.enter_as_added
    txt = Clipboard[0]

    # If it has amount after " = ", make nure only they are added
    if txt =~ / = \$?\d/
      txt.gsub! /.* = /, ''
    end

    # If it has dollars, just add dollars
    if txt =~ /\$\d/
      View.insert self.sum_dollars(txt).to_s
      return
    end

    View.insert self.sum_numbers(txt).to_s

  end
end
