class GitDocs
  def self.menu_after output, *args
    return if output   # If menu outputted something do nothing

    # Menu didn't output, so we need to show manual entry
    command = args[-1]

    # Prepend"git-" (except for gitweb command)
    command = "git-#{command}" if ! ["gitweb", "gitk"].member? command

    txt = Console.sync "man #{command}", :clean=>1

    txt.sub! /\n\n\n+/, "\n\n"   # Remove first group of multiple linebreaks

    yield[:no_search] = 1
    Tree.quote txt
  end
end
