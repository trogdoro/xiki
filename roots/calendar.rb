class Calendar

  def self.menu month=nil
    options = yield
    task = options[:task]

    # Handle tasks...

    return "* year" if options[:task] == []

    if options[:task] == ["year"]
      return "- jan/\n- feb/\n- mar/\n- apr/\n- may/\n- jun/\n- jul/\n- aug/\n- sep/\n- oct/\n- nov/\n- dec/"
    end

    now = Time.now
    no_month = month.nil?   # Remember whether there was no month
    month ||= now.month   # If no month, assume current

    txt = self.month month, now.year
    txt = "^ options\n- #{now.strftime("%b").downcase}/\n#{txt.gsub /^/, '  '}" if no_month   # Add more options
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
