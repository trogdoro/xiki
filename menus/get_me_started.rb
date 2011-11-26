class GetMeStarted
  def self.menu *args

    View.glow "- Adding a few sample items...", :times=>3

    Tree.to_parent
    Tree.kill_under
    Xiki.dont_search
    Tree << "
      | Modify this sample menu to your liking.
      |
      | Then you can do as+menu (type Ctrl-a Ctrl-m) to save it and
      | update it in the future.
      |
      - sample menu/
        - another/
        - and another/
      "
    nil
  end
end
