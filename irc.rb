class Irc

  def self.menu
    "
    - .logon
    - room (o): /j merb
    - identify (o): /m nickserv IDENTIFY #{self.pw}
    "
  end

  def self.logon
    View.handle_bar
    $el.el4r_lisp_eval "(erc :server \"irc.freenode.net\" :nick \"#{@@username}\")"
  end

  def self.username= to;  @@username = to;  end
  def self.username;  @@username;  end

  def self.pw;  @@pw;  end
  def self.pw= to;  @@pw = to;  end

end
