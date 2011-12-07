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

  def self.<< file
    $el.insert_image $el.create_image(file)
  end
end
