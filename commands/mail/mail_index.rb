module Menu
  class Mail
    def self.menu *args
      options = yield

      # /, so show list contacts in =email addresses...

      if args == []
        txt = Xiki["email addresses"].gsub(/^> (.+)/, "- \\1/")
        txt << "\n=email addresses/\n=conf/\n"
        txt << "| Make this use =contacts instead\n=contacts/"
        return txt
      end

      # /name or /email, so show sample message...

      if args.length == 1
        return "
          > The subject
          | The body of
          | the email.
          "
      end

      email = args[0]

      if email !~ /@.+\./   # if not @, get the email address
        email = Xiki["email addresses/> #{email}"]
        email = Tree.unquote(email).strip
      end

      # Read in conf, and complain if not set up...

      conf = options[:conf]
      return "
        > Set conf values first
        =conf/mail/
        " if conf.strip == ""

      # Send the email...

      conf = Xik.new conf
      from_address = conf["from"]

      require 'net/smtp'
      require 'mail'
      require 'gmail_xoauth'

      the_subject = Tree.siblings[0].sub(/^> /, '')
      txt = args[1]

      # Remove linebreaks on lines ending in trailing spaces

      txt.gsub! " \n", " "

      raise "- Expand one of the |... lines!" if txt !~ /\n/

      mail = ::Mail.new do
        from from_address
        to email
        subject the_subject
        body txt  # add an attachment via add_file
      end

      begin
        smtp = Net::SMTP.new('smtp.gmail.com', 587)
        smtp.enable_starttls_auto
        smtp.start('gmail.com', from_address, conf["access token"], :xoauth2) do |smtp|
          smtp.send_message mail.to_s, from_address, email
        end
      rescue Exception=>e

        # If not auth error, error out

        if e.message !~ /\A334/
          return "
            > Todo > find pattern
              | continue on when it looks like the token needs refreshing
            > Error
            | #{e.message}
            "
        end

        # If a "refesh token" in conf already, try to regenerate token ourself.

        if (refresh_token = conf["refresh token"]) && refresh_token != "1/samplesamplesamplesamplesamplesample"

          View.flash "- generating new token from refresh_token!"

          txt = Shell.sync "python oauth2.py --client_id=#{conf['client id']} --client_secret=#{conf['client secret']} --refresh_token=#{refresh_token}",
            :dir=>"#{Xiki.dir}commands/mail/"

          Ol.a txt
          access_token = txt[/Access Token: (.+)/, 1]


          conf_file = File.expand_path "~/xiki/commands/conf/mail.conf"
          conf_txt = File.read conf_file
          conf_txt.sub! /^(- access token\/\n  ).+/, "\\1#{access_token}"
          File.open(conf_file, "w") { |f| f << conf_txt }

          # TODO: call service again? > Or just make them refresh each time?


          return "
            - generated new token from refresh_token!
            | Expand again to send
            "
        end

        return "
          > Oauth access token needs to be updated
          | 1. Run this command:
          =#{Xiki.dir}commands/mail/
            % python oauth2.py --generate_oauth2_token --client_id=#{conf['client id']} --client_secret=#{conf['client secret']}
          | 2. Save it in the conf:
          =conf/mail/
          "

      end

      # "<! sent!"
      "- sent"

    end
  end
end

