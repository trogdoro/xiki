class Xiki::PythonHandler
  include Xiki

  def self.handle options
    source = options[:ex]['py']
    return if ! source || options[:output] || options[:halt]

    # Better api call for this?  Maybe pass 'ex' in through options as well, so we don't need the param.

    source = "#{options[:enclosing_source_dir]}#{source}"
    txt = File.read source

    txt = self.eval txt, options.merge(:file=>source)
    options[:output] = txt
  end

  def self.eval txt, options={}

    txt = "
      import json
      def func():
        args = #{JSON[options[:args] || []]}
      ".unindent+txt.gsub(/^/, "  ")+"\n"+"
      print(json.dumps(func()))
      ".unindent

    txt = Console.run "python -", :sync=>true, :stdin=>txt
    lines = txt.split "\n"

    # Last line is the return value...

    returned = lines.pop
    out = lines.any? ? lines.join("\n") : nil
    Ol.a(out, :stack_line=>"#{options[:file]}:1:in `script'") if out

    if returned == "None"
      nil
    elsif returned !~ /^[{\[]/   # Because JSON has to be an array or hash
      JSON["[#{returned}]"][0]
    else
      JSON[returned]
    end

  rescue Exception=>e
    return CodeTree.draw_exception e
  end
end
