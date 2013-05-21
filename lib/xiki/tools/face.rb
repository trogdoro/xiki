module Xiki
  class Face
    def self.menu part, value=nil
      txt = File.read("/tmp/face.yml") rescue ""
      hash = YAML::load(txt) || {}

      hash[part] = value if value
      hash = {} if part == "reset"
      File.open("/tmp/face.yml", "w") { |f| f << hash.to_yaml }

      html = "
        <html><head><style> img {position:absolute} </style></head><body>
          <img src='/projects/face/img/complexion_blank.png'>
        ".unindent
      hash.keys.sort.each{|key| html << "  <img src='/projects/face/img/#{key}_#{hash[key].gsub ' ', '_'}.png'>\n"}
      html << "</body></html>"

      Browser.html html
    end
  end
end
