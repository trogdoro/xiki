class Calendar
  def self.menu month=nil
    Xiki.dont_search
    #     $xiki_no_search = true
    return "jan/\nfeb/\nmar/\napr/\nmay/\njun/\njul/\naug/\nsep/\noct/\nnov/\ndec/" if month == "year"

    now = Time.now
    no_month = month.nil?   # Remember whether there was no month
    month ||= now.month   # If no month, assume current

    txt = self.month month, now.year
    txt = "#{now.strftime("%b").downcase}/\n#{txt.gsub /^/, '  '}\n<= year/" if no_month   # Add more options
    txt
  end

  def self.month month, year
    now = Time.now   # Start with today

    i = 1
    d = Time.mktime year, month, i
    month = d.month
    is_current = now.year == d.year && now.month == d.month

    result = ""   # Add spaces for missing days
    result << "    " * d.wday

    while d && d.month == month do
      result << "#{d.day.to_s.rjust(2)}  "

      if d.wday == 6
        result << "\n"
      end

      i += 1
      d = Time.mktime(year, month, i) rescue nil
    end

    result.gsub! /^/, "| "
    result.sub!(/ (#{now.day}) /, "(\\1)") if is_current
    result.gsub!(/ +$/, "")   # Remove end of line spaces
  end
end
