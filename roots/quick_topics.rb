# /, so return favorite topics...

if args == []

  return "* edit" if task == []
  return View.open("~/.xiki/misc/favorite/topics.xiki") if task == ["edit"]

  file = File.expand_path "~/.xiki/misc/favorite/topics.xiki"
  txt = File.read(file) rescue nil

  if txt
    options[:hotkey] = 1
  end

  txt ||= "<* - No favorites yet!"
  return txt
end


# /topic, so replace parent and expand...

"<< #{args[0]}"

