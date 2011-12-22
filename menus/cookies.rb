class Cookies
  def self.menu key=nil, val=nil

    txt = Firefox.run "document.cookie"
    hash = txt.split('; ').inject({}) do |acc, e|
      k, v = e.match(/(.+?)=(.+)/)[1..2]
      acc[k] = v
      acc
    end

    # If no args, show all keys

    if key.nil?
      return hash.keys.sort.map{|k| "- #{k}/\n"}.join
    end

    # If key, show its value

    "| #{hash[key]}"
  end
end
