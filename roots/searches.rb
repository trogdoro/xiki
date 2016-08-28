log_file = File.expand_path("~/.xiki/misc/logs/search_log.xiki")

# /, so list history

txt = File.read log_file
txt.split("\n").reverse.uniq.map{|o| "= #{o}"}.join("\n")
