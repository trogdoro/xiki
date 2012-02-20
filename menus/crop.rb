class Crop

  def self.menu coords=nil, save=nil

    dir = Tree.dir :file=>"foo.png"

    tmp_dir = "/tmp/crop"
    Dir.mkdir tmp_dir if ! File.exists? tmp_dir

    # If nothing, show coords options

    return "
      - 40x40+10+10/
      - 200x200+100+100/
      " if ! coords

    dir, file = File.dirname(dir), File.basename(dir)
    dest = file.sub '.', "_#{coords.sub('+', '_')}."

    # If just coords, so crop into tmp dir and show

    if save.nil?
      dest = "#{tmp_dir}/#{dest}"

      Console.sync %`convert "#{file}" -crop #{coords} "#{dest}"`, :dir=>dir
      Image.>> dest, "_"
      return
    end

    # Image clicked on, so crop to destination

    Console.sync %`convert "#{file}" -crop #{coords} "#{dest}"`, :dir=>dir
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
