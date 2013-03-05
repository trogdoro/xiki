class Youtube
  def self.menu

    "
    | Type words to search youtube

    - url with start time/
      @http://www.youtube.com/watch?v=XkYcWzBCEb8#t=2m51s
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

    > Notes
    @technologies/youtube/
    "
  end

  def self.menu_after output, *args
    return output if output

    txt = ENV['txt']

    Browser.url "http://www.youtube.com/results?search_query=#{txt}"
  end
end
