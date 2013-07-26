module Menu
  class Time

    MENU = %`
      - .time zones/
      - see/
        @date/
        @calendar/
        @timer/
      `

    def self.menu_after out, *args
      if args == []
        time = ::Time.now.strftime("%-l:%M:%S%P")
        return "#{time}\n#{out}"
      end

      # /..., so prepend time to top

      nil
    end

    def self.time_zones name=nil
      ['active_support', 'active_support/time', 'active_support/time_with_zone'].each{|o| require o}

      require 'active_support'
      require 'active_support/time'
      require 'active_support/time_with_zone'

      # /, so return list of time zones

      return ActiveSupport::TimeZone.us_zones.map{|o| "#{o.name}/".sub(" (US & Canada)", '')} if ! name

      ActiveSupport::TimeZone.us_zones.each do |o|
        if o.name.sub(" (US & Canada)", '') == name
          return "#{o.now.strftime('%-l:%M:%S%P')}"
        end
      end

    end
  end
end
