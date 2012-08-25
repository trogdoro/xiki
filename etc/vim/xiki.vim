function! XikiLaunch()
  ruby << EOF
    xiki_dir = ENV['XIKI_DIR']
    ['ol', 'vim/line', 'vim/tree'].each {|o| require "#{xiki_dir}/lib/xiki/#{o}"}
    line = Line.value
    indent = line[/^ +/]
    command = "xiki #{line}"
    result = `#{command}`
    Tree << result
EOF
endfunction

nmap <silent> <2-LeftMouse> :call XikiLaunch()<CR>
imap <silent> <2-LeftMouse> <C-c>:call XikiLaunch()<CR>i
imap <silent> <C-CR> <C-c>:call XikiLaunch()<CR>i
nmap <silent> <C-CR> :call XikiLaunch()<CR>
