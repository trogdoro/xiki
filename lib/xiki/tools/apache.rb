module Xiki
  class Apache
    def self.menu
      "
      - .restart/
      - conf/
        @/etc/apache2/
          + other/
          + httpd.conf
      - logs/
        @/var/log/apache2/
          % tail -f error_log
          % tail -f access_log
      "
    end

    def self.restart
      Console.run "sudo apachectl restart", :dir=>"/etc/apache2/"

      "- restarting!"
    end
  end
end
