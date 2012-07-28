require 'ftools'

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
    docs/
      > Show image underneath
      @image//tmp/draw_rect.gif
    api/
      > Examples
      | Insert an image:
      @Image << "/tmp/draw_rect.gif"
    `
  end

  def self.>> file, txt=nil
    txt ||= "@img/#{file}"
    Move.to_end
    self.<< file, txt, :enter_junior=>1
    Move.backward
  end

  def self.<< file, txt=nil, options={}
    txt ||= "@img/#{file}"

    # Copy to place with unique name, so a cached version doesn't get displayed...
    tmp_dir = "/tmp/img_tmp/"
    Dir.mkdir tmp_dir if ! File.directory? tmp_dir

    # Copy to place with unique name, so a cached version doesn't get displayed.

    unique_tmp_file = "#{tmp_dir}#{File.basename(file).sub /.+\./, "\\0#{rand(99999)}."}"

    File.copy file, unique_tmp_file

    Notes.enter_junior if options[:enter_junior]

    $el.insert_image $el.create_image(unique_tmp_file), txt
  end
end
