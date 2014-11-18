# disk/, so show command output...

df_raw = `df -H`

if ! args[0]

  # Extract from 1st line like this:
  # /dev/disk0s2    250G   109G   141G    44% 26641071 34428369   44%   /
  df = df_raw[/\d+G +.+%/, 0].split(/ +/)

  df = "
    | #{df[3]} full
    | (#{df[2]} of #{df[0]} available)
    ".unindent.strip

  return "#{df}\n- all volumes/"

end


# disk/all volumes/, so show command output...

if args[0] == "all volumes"
  return Tree.quote df_raw
end

"nothing"
