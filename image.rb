class Image
  def self.menu_before *args
    if args[-1] =~ /\.(png|gif|jpg)\/?$/
      path = args.join('/')
      Notes.enter_junior
      Image << path
      View.cursor = Line.right - 1
      return false   # Tell it to not continue to menu
    end
  end

  def self.menu
    %`
    .log/
    .docs/
      > Examples
      | Insert an image:
      @p Image << "/Users/craig/Desktop/star.png"
    `
  end

  def self.>> file
    Move.to_end
    Notes.enter_junior
    self.<< file
    Move.backward
  end

  def self.<< file, txt=" "
    $el.insert_image $el.create_image(file), txt
  end
end
