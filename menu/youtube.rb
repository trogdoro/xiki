class Youtube
  def self.menu

    "
    > Type some words to search youtube
    | Dancing Cats

    - url with start time/
      @ http://www.youtube.com/watch?v=XkYcWzBCEb8#t=2m51s
    - use dotsies in transcript/
      @css/
        | .cptime, .cptext{
        |   font-weight: 300;
        | }
        | .cptext{
        |   font-family: dotsies;
        |   padding-left: 1px;
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
