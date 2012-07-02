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
    return self.docs if args == ['docs']

    path = Tree.file :require=>1
    Tree.quote Console.sync "ls -lah \"#{path}\"", :dir=>path
  end
end
