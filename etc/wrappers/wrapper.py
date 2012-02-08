# Gets shelled out to by xiki to delegate call to a .py file.
# Just gets the args passed in and requires and invokes.
import sys, re

file = sys.argv[1]
path = sys.argv[2]

clazz = re.sub(".+/", "", file)
clazz = re.sub("\.py$", "", clazz)
clazz = clazz.capitalize()
execfile(file)

method = ".menu"

import re

if re.search("^\.", path):
  method = path.replace(r'/', '')

print(eval(clazz+"()"+method+"()"))
