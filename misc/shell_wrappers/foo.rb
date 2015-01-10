# Let's say a command looks like this:
# $ foo
#   : bar
#   : bah
#
# If a user expands bah, then the args variable will
# contain [": bah"], so this block will be run:

if args == [": bah"]
  # Just return a string, to insert it underneath.
  return ": ram\n: ewe"
end

# Then the command looks like this:
# $ foo
#   : bar
#   : bah
#     : ram
#     : ewe
#
# If a user later expands ewe, args will contain
# [": bah", ": ewe"].

if args == [": bah", ": ewe"]
  return ": what did you say?"
end

# Return something if they expanded any other item
": You expanded #{args[-1]}"
