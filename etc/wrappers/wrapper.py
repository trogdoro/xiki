# Gets shelled out to by xiki to delegate call to a .py file.
# Just gets the args passed in and requires and invokes.
import sys, re
file = sys.argv[1]
clazz = re.sub(".+/", "", file)
clazz = re.sub("\.py$", "", clazz)
clazz = clazz.capitalize()
path = sys.argv[2]
execfile(file)
print(eval(clazz+"().menu()"))
