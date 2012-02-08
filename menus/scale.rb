class Scale

  def self.menu percent=nil, save=nil
Ol << "save: #{save.inspect}"

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
Ol << "dir: #{dir.inspect}"
Ol << "file: #{file.inspect}"
    dest = file.sub '.', "_#{percent.sub('%', '')}."
Ol << "dest: #{dest.inspect}"

Ol << "percent: #{percent.inspect}"
Ol << "dir: #{dir.inspect}"

    # If just percent, so resize into tmp dir and show

    if save.nil?
Ol << "!"
      dest = "#{tmp_dir}/#{dest}"
Ol << "dest: #{dest.inspect}"
      Console.sync %`convert "#{file}" -resize #{percent} "#{dest}"`, :dir=>dir
      Image.>> dest, "_"
      #       Line.previous
      return
    end

    # Image clicked on, so resize to destination

Ol << "!"
    Console.sync %`convert "#{file}" -resize #{percent} "#{dest}"`, :dir=>dir
    Line.previous
    Tree.to_parent
    Tree.kill_under
    indent = Line.indent Line.value(0)
    Line.next
    View.<< "#{indent}- #{dest}\n", :dont_move=>1
    Effects.glow :what=>:line #, :times=>2
    #     Effects.glow :color=>:fire, :what=>:line

    #   @Effects.glow :color=>:fire
    #   @Effects.glow :color=>:water
    #   @Effects.glow :color=>:forest
    #   @Effects.glow :color=>:rainbow
    #   @Effects.glow :color=>:fat_rainbow

    nil

    #     %`
    #     | TODO: implement
    #     | For now, just run command yourself
    #     $ convert "#{file}" -resize #{percent} resized_"#{file}"
    #     `

  end
end
