class PythonHandler
  def self.handle options
    source = options[:ex]['py']
    return if ! source || options[:output] || options[:halt]

    # Better api call for this?  Maybe pass 'ex' in through options as well, so we don't need the param.

    source = "#{options[:last_source_dir]}#{source}"
    code = File.read source
    code = "args = #{(options[:args]||[]).inspect}\n#{code}"

    txt = Console.run "python -", :sync=>true, :stdin=>code
    options[:output] = txt
  end
end
