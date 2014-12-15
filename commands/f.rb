# First, maybe find code that does "##"
# and use =f instead?
# (Make launcher use @f as special case if file tree.)
class F
  def self.menu *args

    options = yield
    ancestors = options[:ancestors]

    # Stitch our args onto the ancestor before calling!
    ancestors[-1] << "#{args[1..-1].join('/')}/" if args.length > 1

    txt = Expander.expand ancestors

    return txt if args.length > 1   # If there were args, just echo through (filtering happened at higher level

    txt = txt.split("\n").grep(/#{args[0]}/i).join("\n")

    txt

    # TODO > handle multiple filter args

  end
end
