class Apache
  def self.menu
    "
    - .restart/
    - conf/
      @/usr/local/etc/apache2/
        + other/
        + httpd.conf
    - logs/
      @/var/log/apache2/
        % tail -f error_log
        % tail -f access_log
    "
  end

  def self.restart
    Console.run "sudo apachectl restart", :dir=>"/usr/local/etc/apache2/"

    "- restarting!"
  end
end
