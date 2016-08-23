# /, so give example...

if args == []
  return '
    > Edit this svg and expand to set your desktop image
    | <rect x="0" y="0" width="1000" height="1000" style="fill:#124"/>
    | <circle id="t" cx="300" cy="450" r="140" style="fill:white; opacity:0.08" />
    | <circle id="t" cx="600" cy="400" r="200" style="fill:white; opacity:0.08" />
  '
end

# /svg code, so set it to desktop...
txt = '
  <rect x="0" y="0" width="1000" height="1000" style="fill:#142"/>
  <circle id="t" cx="200" cy="50" r="40" style="fill:white; opacity:0.1" />
  <circle id="t" cx="300" cy="100" r="100" style="fill:white; opacity:0.1" />
'
txt = args[0]

xml = %`
  <?xml version="1.0" encoding="UTF-8" standalone="no"?>
  <svg xmlns="http://www.w3.org/2000/svg" version="1.1" height="1000" width="1000">
  #{txt}
  </svg>
  `.strip

tmp_path = "/tmp/tmp.svg"
File.open(tmp_path, "w") { |f| f << xml }

png = "/tmp/desktop#{rand 99999}.png"

`convert #{tmp_path} #{png}`

Applescript.run %`
  tell application "System Events" to set picture of every desktop to "#{png}"
`.strip

"<! set desktop"
