#
# Shows your operating system and version.
#

# /etc/os-release exists, so use it (probably linux)...

if txt = File.read("/etc/os-release") rescue nil
  return Tree.pipe txt[/.+\n.+\n/]   # Show the 1st 2 lines
end

# sw_vers command exists, so use it (probably mac)...

if txt = Shell.command("sw_vers", :raise_error=>1) rescue nil
  return Tree.pipe txt[/.+\n.+\n/]   # Show the 1st 2 lines
end

"
| Sorry, your OS was unrecognized, because neither
| the /etc/os-release file nor the 'sw_vers' shell
| command could be found to give clues about your
| OS and its version.
|
"
