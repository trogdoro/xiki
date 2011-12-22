class Say
  def self.menu *args
    txt = args.join('/').gsub('"', '\"')
    $el.do_applescript "say \"#{txt}\""
  end
end
