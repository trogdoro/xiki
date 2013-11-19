module Menu
  class Date

    MENU = %`
      - .short/
      - see/
        @calendar/
        @time/
      `

    def self.menu_after out, *args

      if args == []
        return "| #{::Time.now.strftime("%B %-m, %Y")}\n#{out}"
      end

      # /..., so prepend time to top

      nil
    end

    def self.short
      "| #{::Time.now.strftime("%Y-%m-%d")}"
    end


  end
end


# - local/
#   ! Time.now.strftime("%l:%M:%S %p %Z")
# - time zone/
#   ! zone = args[0]
#   ! if ! zone
#   !   return ActiveSupport::TimeZone.us_zones.map(&:name).map{|o| "- #{o}/\n"}.join('')
#   ! end
#   !
#   ! zone.sub! /^\| /, ''
#   ! t = Time.now.in_time_zone ActiveSupport::TimeZone.new(zone)
#   ! t.strftime("%l:%M:%S %p %Z")
# - city/
#   @google/what time is it in Chicago?
