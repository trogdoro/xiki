class MenuHandler
  def self.handle options, ex
    return if ! ex['menu'] || options[:output] || options[:halt]
    file = "#{options[:last_source_dir]}#{ex['menu']}"
    txt = File.read file

    path = (options[:args]||[]).join "/"

    txt = Tree.children txt, path
    self.eval_when_exclamations txt

    options[:output] = txt
  end

  # If started with !, eval code...
  def self.eval_when_exclamations txt

    return if Keys.prefix == "source"

    return if txt !~ /^! /

    # TODO: to tighten up, only do this if all lines start with "!"

    code = txt.gsub /^! /, ''
    returned, out, exception = Code.eval code

    txt.replace(
      if exception
        CodeTree.draw_exception exception, code
      else
        returned || out   # Otherwise, just return return value or stdout!"
      end
      )
  end
end
