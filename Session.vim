let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <C-U> u
map! <S-Insert> *
vmap  "*d
nmap <silent> % <Plug>(MatchitNormalForward)
xmap <silent> % <Plug>(MatchitVisualForward)
omap <silent> % <Plug>(MatchitOperationForward)
map Q gq
nmap <silent> [% <Plug>(MatchitNormalMultiBackward)
xmap <silent> [% <Plug>(MatchitVisualMultiBackward)
omap <silent> [% <Plug>(MatchitOperationMultiBackward)
nmap <silent> ]% <Plug>(MatchitNormalMultiForward)
xmap <silent> ]% <Plug>(MatchitVisualMultiForward)
omap <silent> ]% <Plug>(MatchitOperationMultiForward)
xmap a% <Plug>(MatchitVisualTextObject)
nmap <silent> g% <Plug>(MatchitNormalBackward)
xmap <silent> g% <Plug>(MatchitVisualBackward)
omap <silent> g% <Plug>(MatchitOperationBackward)
vmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>(MatchitNormalForward) :call matchit#Match_wrapper('',1,'n')
nnoremap <silent> <Plug>(MatchitNormalBackward) :call matchit#Match_wrapper('',0,'n')
xnoremap <silent> <Plug>(MatchitVisualForward) :call matchit#Match_wrapper('',1,'v')m'gv``
xnoremap <silent> <Plug>(MatchitVisualBackward) :call matchit#Match_wrapper('',0,'v')m'gv``
onoremap <silent> <Plug>(MatchitOperationForward) :call matchit#Match_wrapper('',1,'o')
onoremap <silent> <Plug>(MatchitOperationBackward) :call matchit#Match_wrapper('',0,'o')
nnoremap <silent> <Plug>(MatchitNormalMultiBackward) :call matchit#MultiMatch("bW", "n")
nnoremap <silent> <Plug>(MatchitNormalMultiForward) :call matchit#MultiMatch("W",  "n")
xnoremap <silent> <Plug>(MatchitVisualMultiBackward) :call matchit#MultiMatch("bW", "n")m'gv``
xnoremap <silent> <Plug>(MatchitVisualMultiForward) :call matchit#MultiMatch("W",  "n")m'gv``
onoremap <silent> <Plug>(MatchitOperationMultiBackward) :call matchit#MultiMatch("bW", "o")
onoremap <silent> <Plug>(MatchitOperationMultiForward) :call matchit#MultiMatch("W",  "o")
xmap <silent> <Plug>(MatchitVisualTextObject) <Plug>(MatchitVisualMultiBackward)o<Plug>(MatchitVisualMultiForward)
vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(netrw#GX(),netrw#CheckIfRemote(netrw#GX()))
vmap <C-X> "*d
vmap <C-Del> "*d
vmap <S-Del> "*d
vmap <C-Insert> "*y
vmap <S-Insert> "-d"*P
nmap <S-Insert> "*P
inoremap  u
let &cpo=s:cpo_save
unlet s:cpo_save
set backspace=indent,eol,start
set backup
set display=truncate
set encoding=utf-8
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set guifont=Courier_New:h10:cRUSSIAN:qDRAFT
set guioptions=egmrLT
set helplang=ru
set history=200
set hlsearch
set incsearch
set langnoremap
set nolangremap
set mouse=nvi
set nrformats=bin,hex
set ruler
set runtimepath=~/vimfiles,C:\\Program\ Files\ (x86)\\Vim/vimfiles,C:\\Program\ Files\ (x86)\\Vim\\vim82,C:\\Program\ Files\ (x86)\\Vim\\vim82\\pack\\dist\\opt\\matchit,C:\\Program\ Files\ (x86)\\Vim/vimfiles/after,~/vimfiles/after
set scrolloff=5
set shiftwidth=4
set tabstop=4
set ttimeout
set ttimeoutlen=100
set undofile
set wildmenu
set window=55
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~\Desktop\haskell\server
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
argglobal
%argdel
edit package.yaml
set splitbelow splitright
wincmd _ | wincmd |
split
wincmd _ | wincmd |
split
2wincmd k
wincmd w
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd w
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe '1resize ' . ((&lines * 0 + 28) / 56)
exe '2resize ' . ((&lines * 36 + 28) / 56)
exe 'vert 2resize ' . ((&columns * 40 + 117) / 235)
exe '3resize ' . ((&lines * 36 + 28) / 56)
exe 'vert 3resize ' . ((&columns * 194 + 117) / 235)
exe '4resize ' . ((&lines * 16 + 28) / 56)
exe 'vert 4resize ' . ((&columns * 40 + 117) / 235)
exe '5resize ' . ((&lines * 16 + 28) / 56)
exe 'vert 5resize ' . ((&columns * 194 + 117) / 235)
argglobal
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal completeslash=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal cursorlineopt=both
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'yaml'
setlocal filetype=yaml
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,128-167,224-235
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=4
setlocal noshortname
setlocal showbreak=
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'yaml'
setlocal syntax=yaml
endif
setlocal tabstop=4
setlocal tagcase=
setlocal tagfunc=
setlocal tags=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal undofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal wincolor=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 29 - ((0 * winheight(0) + 0) / 0)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
29
normal! 011|
wincmd w
argglobal
if bufexists("E:\server_config\usr_config.cfg") | buffer E:\server_config\usr_config.cfg | else | edit E:\server_config\usr_config.cfg | endif
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal completeslash=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal cursorlineopt=both
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'cfg'
setlocal filetype=cfg
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,128-167,224-235
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=4
setlocal noshortname
setlocal showbreak=
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'cfg'
setlocal syntax=cfg
endif
setlocal tabstop=4
setlocal tagcase=
setlocal tagfunc=
setlocal tags=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal undofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal wincolor=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 016|
wincmd w
argglobal
if bufexists("package.yaml") | buffer package.yaml | else | edit package.yaml | endif
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal completeslash=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal cursorlineopt=both
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'yaml'
setlocal filetype=yaml
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,128-167,224-235
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal nonumber
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=4
setlocal noshortname
setlocal showbreak=
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'yaml'
setlocal syntax=yaml
endif
setlocal tabstop=4
setlocal tagcase=
setlocal tagfunc=
setlocal tags=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal undofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal wincolor=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 28 - ((9 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
28
normal! 06|
wincmd w
argglobal
if bufexists("plan.txt") | buffer plan.txt | else | edit plan.txt | endif
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal completeslash=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal cursorlineopt=both
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'text'
setlocal filetype=text
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,128-167,224-235
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=4
setlocal noshortname
setlocal showbreak=
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'text'
setlocal syntax=text
endif
setlocal tabstop=4
setlocal tagcase=
setlocal tagfunc=
setlocal tags=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal undofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal wincolor=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 19 - ((6 * winheight(0) + 8) / 16)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
19
normal! 028|
wincmd w
argglobal
if bufexists("app\Main.hs") | buffer app\Main.hs | else | edit app\Main.hs | endif
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal completeslash=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal cursorlineopt=both
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'haskell'
setlocal filetype=haskell
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,128-167,224-235
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=4
setlocal noshortname
setlocal showbreak=
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'haskell'
setlocal syntax=haskell
endif
setlocal tabstop=4
setlocal tagcase=
setlocal tagfunc=
setlocal tags=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal undofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal wincolor=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 50 - ((15 * winheight(0) + 8) / 16)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
50
normal! 031|
wincmd w
3wincmd w
exe '1resize ' . ((&lines * 0 + 28) / 56)
exe '2resize ' . ((&lines * 36 + 28) / 56)
exe 'vert 2resize ' . ((&columns * 40 + 117) / 235)
exe '3resize ' . ((&lines * 36 + 28) / 56)
exe 'vert 3resize ' . ((&columns * 194 + 117) / 235)
exe '4resize ' . ((&lines * 16 + 28) / 56)
exe 'vert 4resize ' . ((&columns * 40 + 117) / 235)
exe '5resize ' . ((&lines * 16 + 28) / 56)
exe 'vert 5resize ' . ((&columns * 194 + 117) / 235)
tabnext 1
badd +1 package.yaml
badd +1 E:\server_config\usr_config.cfg
badd +1 conf.cfg
badd +0 plan.txt
badd +1 app\Main.hs
badd +12 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\conf.cfg
badd +52 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\HTTP\Impl\TG.hs
badd +7 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Impl\TG.hs
badd +124 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\TG.hs
badd +35 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Log\Handle.hs
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\log.log
badd +205 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\VK.hs
badd +19 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\app\Main.hs
badd +37 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Impl\Common.hs
badd +4 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Log\Impl\BotLog.hs
badd +8 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\HTTP\Impl\VK.hs
badd +45 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\HTTP\Handle.hs
badd +11 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Reader.hs
badd +81 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\JSON\TG.hs
badd +40 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Impl\VK.hs
badd +19 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Internal.hs
badd +20 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\VK.hs
badd +40 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Configuration.hs
badd +11 \Users\Demo\Desktop\haskell\test\testParseRequest\app\Main.hs
badd +35 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\package.yaml
badd +14 \Users\Demo\Desktop\haskell\repeat-aeson\app\Main.hs
badd +40 \Users\Demo\Desktop\haskell\repeat-aeson-II\app\Main.hs
badd +5 \Users\Demo\Desktop\haskell\learn-aeson\app\Main.hs
badd +50 \Users\Demo\Desktop\haskell\readConfig\app\Main.hs
badd +36 \Users\Demo\Desktop\haskell\test\test.hs
badd +5 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Lib.hs
badd +1 \Users\Demo\Desktop\haskell\repeat-aeson\myJSON.json
badd +1 \Users\Demo\Desktop\haskell\repeat-aeson\Main.hs
badd +23 \Users\Demo\Desktop\haskell\learn-aeson\package.yaml
badd +4 \Users\Demo\Desktop\haskell\learn-aeson\myJSON.json
badd +7 \Users\Demo\Desktop\haskell\learn-aeson\myJSON_m.json
badd +2 \Users\Demo\Desktop\haskell\learn-aeson\myJSON_nm.json
badd +15 \Users\Demo\Desktop\haskell\repeat-aeson\package.yaml
badd +29 \Users\Demo\Desktop\haskell\repeat-aeson\app\Main.hs.bak
badd +51 \Users\Demo\Desktop\haskell\learn-aeson-II\app\Main.hs
badd +5 \Users\Demo\Desktop\haskell\learn-aeson-II\myJSON.json
badd +24 \Users\Demo\Desktop\haskell\learn-aeson-II\package.yaml
badd +6 \Users\Demo\Desktop\haskell\learn-aeson-II\map.json
badd +14 \Users\Demo\Desktop\haskell\repeat-aeson-II\field.json
badd +1 \Users\Demo\Desktop\haskell\learn-aeson-II\fiedl.json
badd +1 \Users\Demo\Desktop\haskell\learn-aeson-II\field.json
badd +24 \Users\Demo\Desktop\haskell\repeat-aeson-II\package.yaml
badd +1 \Users\Demo\Desktop\haskell\test\request.txt
badd +1 \Users\Demo\Desktop\haskell\test\rk.txt
badd +1 \Users\Demo\Desktop\haskell\test\parseRequest.txt
badd +3 \Users\Demo\Desktop\haskell\test\parseRequest.hs
badd +25 \Users\Demo\Desktop\haskell\test\testParseRequest\package.yaml
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\TH.hs
badd +3 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\TG.hs
badd +3 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\Reader.hs
badd +4 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\VK_Handle.hs
badd +24 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\VK_Handle.hs
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Conf\Reader.hs
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Impl\VK_Handle.hs
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Impl\TH.hs
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\Conf\Impl\TG
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\JSON\TH.hs
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\HTTP\TG.hs
badd +3 \Users\Demo\Desktop\haskell\bot-VK-T\tg-response.json
badd +2 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\src\HTTP\Handle.hs~
badd +1 \Users\Demo\Desktop\haskell\bot-VK-T\bot-VK-T\srс\Log\Handle.hs
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOS
set winminheight=1 winminwidth=1
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
