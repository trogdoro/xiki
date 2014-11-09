module Xiki
  class Cookies
    def self.menu key=nil, val=nil

      txt = Firefox.exec "document.cookie"

      return "- no cookies set!" if txt == '""'

      hash = txt.split('; ').inject({}) do |acc, e|
        k, v = e.match(/(.*?)=(.*)/)[1..2]
        acc[k] = v
        acc
      end

      # If no args, show all keys

      if key.nil?
        return hash.keys.sort.map{|k| "- #{k}/\n"}.join
      end

      # If just key, show its value
      if val.nil?
        return "| #{hash[key]}"
      end

      "<! updated!"

    end
  end
end
