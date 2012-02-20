class Scale

  def self.menu percent=nil, save=nil

    dir = Tree.dir :file=>"foo.png"

    tmp_dir = "/tmp/scale"
    Dir.mkdir tmp_dir if ! File.exists? tmp_dir

    # If nothing, show percent options

    return "
      - 25%/
      - 50%/
      - 75%/
      - 150%/
      - 64x64/
      - 128x128/
      " if ! percent

    dir, file = File.dirname(dir), File.basename(dir)
    dest = file.sub '.', "_#{percent.sub('%', '')}."

    # If just percent, so resize into tmp dir and show

    if save.nil?
      dest = "#{tmp_dir}/#{dest}"
      Console.sync %`convert "#{file}" -resize #{percent} "#{dest}"`, :dir=>dir
      Image.>> dest, "_"
      #       Line.previous
      return
    end

    # Image clicked on, so resize to destination

    Console.sync %`convert "#{file}" -resize #{percent} "#{dest}"`, :dir=>dir
    Line.previous
    Tree.to_parent
    Tree.kill_under
    indent = Line.indent Line.value(0)
    Line.next
    View.<< "#{indent}- #{dest}\n", :dont_move=>1

    Effects.glow :color=>:forest, :what=>:line, :times=>1

    nil

  end
end
