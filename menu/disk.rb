# Return something like "28% full (168Gi available)"
df = `df -h`[/\d+Gi +.+%/, 0].split(/ +/)
"
| #{df[3]} full
| (#{df[2]} of #{df[0]} available)
"
