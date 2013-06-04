function! XikiLaunch()
  ruby << EOF
    include Xiki
    xiki_dir = ENV['XIKI_DIR']
    ['core/ol', 'vim/line', 'vim/tree'].each {|o| load "#{xiki_dir}/lib/xiki/#{o}.rb"}
    line = Line.value
    next_line = Line.value 2

    indent = line[/^ +/]
    command = "xiki #{line}"
    result = `#{command}`
    Tree << result
EOF
endfunction

imap <silent> <2-LeftMouse> <C-c>:call XikiLaunch()<CR>i
nmap <silent> <2-LeftMouse> :call XikiLaunch()<CR>
imap <silent> <C-CR> <C-c>:call XikiLaunch()<CR>i
nmap <silent> <C-CR> :call XikiLaunch()<CR>
imap <silent> <C-@> <C-c>:call XikiLaunch()<CR>i
nmap <silent> <C-@> :call XikiLaunch()<CR>

