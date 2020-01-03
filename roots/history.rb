log_file = File.expand_path("~/.xiki/misc/logs/topic_log.xiki")

# ~, so show options

option_item = task

return "* edit" if option_item == []
if option_item == ["edit"]
  return View.open log_file
end


# /, so list history

txt = File.read log_file
txt.split("\n").reverse.uniq.map{|o| "= #{o}"}.join("\n")
