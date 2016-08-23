class Ls
  def self.docs
    "
    > Summary
    | Simple wrapper around the 'ls' shell command. Can be used in trees.

    > Example usage
    @/tmp/
      @ls
    "
  end

  def self.menu *args
    options = yield
    dir = options[:dir]

    return self.docs if args == ['docs']

    #     path = Tree.file :require=>1
    txt = Shell.sync "ls -Alh", :dir=>dir
    #     txt = Shell.sync "ls -Alh \"#{path}\"", :dir=>dir
    txt.sub! /\Atotal.+\n/, ''
    Tree.quote txt
  end
end
