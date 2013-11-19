module Xiki
  class Youtube
    MENU = "
      | Put words here to search on youtube
      - url with start time/
        @http://www.youtube.com/watch?v=XkYcWzBCEb8#t=2m51s
        | Or, with end time:
        @http://www.youtube.com/v/bUR_eUVcABg?start=15&end=20&version=3&autoplay=1
      - use dotsies in transcript/
        @css/
          | .caption-line-text{
          |   font-family: dotsies;
          |   padding-left: 1px;
          | }
          | .caption-line-highlight{
          |   font-family: dotsies;
          |   padding-left: 1px;
          |   font-weight: normal;
          | }
      @notes/
      "

    def self.menu_after output, *args
      return output if output

      txt = yield['args'][0]

      Browser.url "http://www.youtube.com/results?search_query=#{txt}"
    end
  end
end
