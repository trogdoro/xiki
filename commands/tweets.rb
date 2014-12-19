module Menu
  class Tweets

    MENU = '
      =tweet/
      - mine/
        ! n = args[0] =~ /\\A\\d\\z/ ? args[0] : 20
        ! Tree.quote Shell.sync("t timeline -n\#{n} \#{Menu::Tweets.active_account}")
      - followers/
        ! # Shell.sync("t followers").gsub(/^/, "@")
        ! # TEMP > hard-coded!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! txt = ": 1895tillie\n: 22clang\n: 614forHAITI\n: 7imon7ays\n: AA55h\n: acolchado\n: aculich\n: adelpreore\n: afathman\n: agilous\n: aguynamedben\n: alejnaser\n: alex_bolotov\n: alexmoore\n: aljamiat\n: alycit\n: argami_\n: ArLinkinpart\n: artus_lla\n: badkittyx2\n: balasaheb\n: Barber_285\n: bdittmer\n: Beatissima\n: BeerOfTheDay\n: begriffs\n: bengl\n: billmelvin\n: BillSeitz\n: blinkdaddy\n: Bloomtastic\n: BoardAdvice\n: boombuckets\n: brainmuffin\n: brianball\n: bryceglass\n: bwsr_sr\n: bzzbzzapp\n: CarlosMolanoS\n: cbusbiz\n: cbusjs\n: chadarimura\n: chendo\n: CherilyYFrah\n: chipashby\n: ChirpCoin\n: choppen5\n: Chris_Gormley\n: cmordue\n: codiak131\n: ColsUnderground\n: ColumbusGeeks\n: columbusrb\n: COverge\n: CuriousAgilist\n: curiousscholar\n: cyberneticlove\n: DanniFriedland\n: Daravz48\n: darkslategrey\n: dbrans\n: densone\n: devarispbrown\n: dicksontam\n: DineOriginals\n: dirkdk\n: doolin\n: dotemacs\n: dparfrey\n: drnugent\n: dsilver829\n: dysoncjcw\n: dzachrich\n: Eccra\n: ednacrist_\n: Egolabel\n: Eileen_QCon\n: emacs\n: emacsrookie\n: EmptyGreenPants\n: enriquez\n: EpicDash_\n: ericEsilva\n: ericrius1\n: ErikTrautman\n: EseneMerian\n: EstelleDyer905\n: estill01\n: esumerfd\n: esurov\n: etayluz\n: EvaGabriela_Z\n: featherart\n: felhobacsi\n: felixflores\n: fiachetti\n: FionabondW\n: fmcauley\n: fmillysxvy\n: ForecastWatch\n: garethdiz\n: ghepting\n: gilbrea\n: gmichaelsbistro\n: godber\n: grandviewcoffee\n: gregmalcolm\n: griscz\n: guyroyse\n: HackerDudes\n: hackerpreneur\n: haleyrealtor\n: Happensapp\n: HassieMerfeldPI\n: hcatlin\n: HendrikThompson\n: hepcatrayo\n: hexmanshu\n: hibariya\n: HiDanielG\n: hunegnaw\n: hungkienluu\n: iamleppert\n: idealadvisor\n: ienjoythebeach\n: ierceg\n: ImaniLove1\n: indiegamegirl1\n: informatom\n: InvoiceSharing\n: JackGrass1\n: jakedahn\n: jakeseip\n: jamin4jc\n: jarridb\n: jasonong\n: jefflembeck\n: jeshua\n: jessas\n: jfleong\n: jherber\n: jikpeter\n: jimweirich\n: joe_joeman24\n: JoelMcCracken\n: join_choir\n: joncanady\n: jonhogue\n: jonlai\n: jonmyers\n: josephjwilliams\n: jschairb\n: jscheur\n: jscoobysnack\n: kaliaparijat\n: Kamek437\n: KeiperKevin\n: keithtom\n: kevinhsmitty\n: kotp\n: krohrbaugh\n: kylembrandt\n: Lancaster_608\n: laripk\n: ldewald\n: LenJaffe\n: LewisHowes\n: lifeinchords\n: lillchan\n: linuxoncrack\n: lldong\n: LookAroundiVR\n: loriruff\n: ludmal\n: makeparallels\n: marjanpanic\n: markd\n: markhneedham\n: martyhines\n: maxisnow\n: mdpatrick\n: MeganSpeir\n: Memorize\n: micheal\n: mike_critz\n: MikeBlackwell\n: mikedoel\n: mikepgee\n: millrn\n: MissTavakoli\n: morenoh149\n: muncman\n: mwolfe6\n: mybluewristband\n: n_daugherty\n: neall\n: NenaDjaja\n: nikkiwoodruff\n: notthisbody\n: ohiodoug\n: osurb\n: OwnTownSF\n: parker_evi\n: patpohler\n: paulmakepeace\n: peh803\n: phad\n: pietvanzoen\n: pixelcort\n: pizzacity\n: plexus\n: pri1229\n: pritchie\n: protonight\n: QConSF\n: qrush\n: rayashlal\n: rayaytonjones\n: raytrask\n: Raz0r\n: rbwaia\n: rconf\n: re5et\n: redgraveK\n: ReubenYau\n: richard_efc\n: richszeto\n: rissem\n: RobertReiz\n: rpressanti\n: Ry_Nomad\n: sanbor\n: SandboxPowell\n: sascha_vogt\n: Saterus\n: sdemjanenko\n: seekant\n: sidazhang\n: slajax\n: slma617\n: smasry\n: SPORTS52\n: stephbreslin\n: Stephenitis\n: superchris\n: sutrosoftware\n: SydSF\n: tastyc0de\n: tedcooke\n: teespring\n: tennety\n: tess1768\n: TheBitterEntrep\n: theguigirl\n: thetruthcbus\n: thomaar\n: tibbon\n: tobinharris\n: toddsiegel\n: tonywhelan\n: tonywok\n: Toobla\n: TRWerle\n: UnionSqLive\n: VersionEye\n: vigobronx\n: walteryu\n: websiteweekend\n: wyliemac\n: X1011_\n: xandrews\n: xiki\n: yogsototh\n"
        ! sleep 1.8
        ! txt.gsub(/^: /, "@")
      - followees/
        ! Shell.sync("t followees").gsub(/^/, "@")
      - .search/
      - favorites/
        ! Tree.quote Shell.sync("t favorites -n100")
      - .switch account/
      - more/
        - all commands/
          =$ t
        - timeline/
          ! # t timeline [USER]    # Returns the 20 most recent Tweets pos...
          ! Shell.sync "t timeline"
        - config for t gem/
          =~/.trc
        - user info/
          =$ t whois @xiki
        - follow users/
          =$ t follow @sferik @gem
        - timeline/
          =$ t timeline
          =$ t timeline -r
          =$ t timeline -r -l
        - search/
          - most recent tweets for a specified query/
            =$ t search all "query"
          - your favorites/
            =$ t search favorites "query"
          - your mentions/
            =$ t search mentions "query"
          - retweets/
            =$ t search retweets "query"
          - yours/
            =$ t search timeline "query"
          - other user\'s timeline/
            =$ t search timeline @sferik "query"
        - misc/
          - whether user follows another/
            =$ t does_follow @ev @sferik
          - all the members of a list/
            =$ t list members -l following-\`date "+%Y-%m-%d"\`
          - your lists/
            =$ t lists -l
          - all friends, ordered by number of followers/
            =$ t friends -l --sort=followers
          - leaders (people you follow who don\'t follow you back)/
            =$ t leaders -l --sort=followers
          - unfollow everyone you follow who doesn\'t follow you back/
           =$ t leaders | xargs t unfollow
      '
    #       =conf/

    #     def self.menu_after output, txt=nil, *extra
    # #Ol["!"]
    #       options = yield

    #       # /, so prepend accounts to tweet from...

    #       if options[:prefix] == "open" && (!txt || txt =~ /\n/)   # as+open, so just open my profile
    #         Browser.url("https://twitter.com/#{self.active_account}")
    #         return ""
    #       end


    #     end

    def self.active_account

      txt = File.read(File.expand_path "~/.trc")
      raise "> No config file yet" if ! txt
      txt = YAML::load(txt)
      txt["configuration"]["default_profile"][0]

    end

    def self.switch_account name=nil
Ol["!"]

      # /accounts/, so list accounts...

      if ! name
        accounts = self.list_accounts
        accounts = accounts.scan(/^\w+/).map{|o| "+ #{o}/\n"}.join
        accounts << "+ add/"
        return accounts
      end

      # /accounts/add, run shell command to add...

      if name == "add"
        Shell.async "t authorize"
        return ""
      end

      # /accounts/name, so set as active...

      Shell.sync "t set active #{name}"

      "<! made it active!"

    end

    def self.list_accounts

      accounts = nil

      begin
        accounts = Xiki::Shell.sync "t accounts"
Ol "accounts", accounts
      rescue Exception=>e

        # No "t" command, so say to gem install...

        if e.message =~ /\ANo such file/
          raise "
            > Install the gem for tweeting
            | You don't have the 't' gem installed yet.
            | Install it:
            =% gem install t
            "
        end
      end

      # No accounts yet, so say run authorize...

      if accounts == ""
        raise "
          > Add your twitter account
          | You don't have any accounts configured yet.
          | Add at least one:
          =% t authorize
          "
      end

      #       accounts = accounts.scan(/^\w+/).map{|o| "+ #{o}/\n"}.join
Ol.a accounts
      accounts
    end

    #     def self.mine
    #       Tree.quote Shell.sync("t timeline -l #{self.active_account}").gsub /^.+?@/, "@"
    #     end

    def self.search term=nil

      # /search, so show request search term...

      return "=prompt/search term" if ! term

      yield[:no_slash] = 1

      # /search/term, so search...

      term.gsub! "'", "'\\''"
      Tree.quote Shell.sync("t search all -l '#{term}'").gsub /^.+?@/, "@"

    end

  end
end

