class MenuHandler
  def self.handle options, ex
    return if ! ex['menu'] || options[:output]
    file = "#{options[:last_source_dir]}#{ex['menu']}"
    txt = File.read file

    path = (options[:args]||[]).join "/"

    txt = Tree.children txt, path

    # If started with !, eval code...
    if txt =~ /^! /
      # TODO: to tighten up, only do this if all lines start with "!"

      code = txt.gsub /^! /, ''
      returned, out, exception = Code.eval code

      txt =
        if exception
          CodeTree.draw_exception exception, code
        else
          returned || out   # Otherwise, just return return value or stdout!"
        end
    end

    options[:output] = txt
  end
end
