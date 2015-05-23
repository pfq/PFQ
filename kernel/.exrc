if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <silent> <SNR>18_yrrecord =YRRecord3()
nnoremap <silent>  :YRReplace '1', 'p'
nnoremap <silent>  :YRReplace '-1', 'P'
noremap s :TCommentAs =&ft_
noremap n :TCommentAs =&ft 
noremap a :TCommentAs 
noremap b :TCommentBlock
vnoremap <silent> r :TCommentRight
vnoremap <silent> i :TCommentInline
nnoremap <silent> r :TCommentRight
onoremap <silent> r :TCommentRight
noremap   :TComment 
noremap <silent> p m`vip:TComment``
vnoremap <silent>  :TCommentMaybeInline
nnoremap <silent>  :TComment
onoremap <silent>  :TComment
noremap ,_s :TCommentAs =&ft_
noremap ,_n :TCommentAs =&ft 
noremap ,_a :TCommentAs 
noremap ,_b :TCommentBlock
xnoremap <silent> ,_r :TCommentRight
nnoremap <silent> ,_r :TCommentRight
snoremap <silent> ,_r :TCommentRight
onoremap <silent> ,_r :TCommentRight
xnoremap <silent> ,_i :TCommentInline
noremap ,_  :TComment 
noremap <silent> ,_p vip:TComment
xnoremap <silent> ,__ :TCommentMaybeInline
nnoremap <silent> ,__ :TComment
snoremap <silent> ,__ :TComment
onoremap <silent> ,__ :TComment
map ,T <Plug>TaskList
nmap ,ihn :IHN
nmap ,is :IHS:A
nmap ,ih :IHS
map ,hoff :%!xxd -r       
map ,hon :%!xxd
noremap ,d "_dd
noremap ,u :Unite buffer file_rec
noremap ,g :grep -r -w  
noremap ,S :%s/\<\>/<Left><Left><Left>
noremap ,s :%s/\<\>/
noremap ,r :%s/\r//g
noremap ,? ?\<\><Left><Left>
noremap ,/ /\<\><Left><Left>
noremap ,c :/\(\<class\>\|\<struct\>\)\s*[a-zA-Z_][a-zA-Z0-9_]*.*\_s*{ 
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
vnoremap D "_d
xnoremap <silent> P :YRPaste 'P', 'v'
nnoremap <silent> P :YRPaste 'P'
xnoremap <silent> d :YRDeleteRange 'v'
nmap gx <Plug>NetrwBrowseX
xnoremap <silent> gC :TCommentMaybeInline
nnoremap <silent> gCc :let w:tcommentPos = getpos(".") | set opfunc=tcomment#OperatorLineAnywayg@$
nnoremap <silent> gC :let w:tcommentPos = getpos(".") | set opfunc=tcomment#OperatorAnywayg@
xnoremap <silent> gc :TCommentMaybeInline
nnoremap <silent> gcc :let w:tcommentPos = getpos(".") | set opfunc=tcomment#OperatorLineg@$
nnoremap <silent> gc :let w:tcommentPos = getpos(".") | set opfunc=tcomment#Operatorg@
nnoremap <silent> gp :YRPaste 'gp'
nnoremap <silent> gP :YRPaste 'gP'
xnoremap <silent> p :YRPaste 'p', 'v'
nnoremap <silent> p :YRPaste 'p'
xnoremap <silent> x :YRDeleteRange 'v'
xnoremap <silent> y :YRYankRange 'v'
noremap <F3> :AssGuard
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cfile>"),0)
nnoremap <silent> <F11> :call conque_term#exec_file()
nnoremap <silent> <SNR>18_yrrecord :call YRRecord3()
map <Right> <Nop>
map <Left> <Nop>
map <Down> <Nop>
map <Up> <Nop>
noremap <F12> :TagbarToggle
noremap <F10> :GundoToggle
noremap <F9> :NERDTreeToggle
noremap <F8> :QFix
noremap <F7> :Make
noremap <F6> :cnext
noremap <F5> :UpdateTags
inoremap s :TCommentAs =&ft_
inoremap n :TCommentAs =&ft 
inoremap a :TCommentAs 
inoremap b :TCommentBlock
inoremap <silent> r :TCommentRight
inoremap   :TComment 
inoremap <silent> p :norm! m`vip:TComment``
inoremap <silent>  :TComment
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
imap ,ihn :IHN
imap ,is :IHS:A
imap ,ih :IHS
cmap ;tf ?^{??(?,/^}/
iabbr __G __GXX_EXPERIMENTAL_CXX0X__
iabbr __F __FUNCTION__
iabbr __P __PRETTY_FUNCTION__
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autowrite
set backspace=indent,eol,start
set cmdheight=2
set complete=.,w,b,t,i
set completeopt=longest,menuone
set copyindent
set cpoptions=aABceFs$
set diffopt=filler,iwhite,context:10
set fileencodings=ucs-bom,utf-8,default,latin1
set grepprg=cgrep
set helplang=en
set hidden
set history=1000
set hlsearch
set incsearch
set laststatus=2
set lazyredraw
set matchpairs=(:),{:},[:],<:>
set nomodeline
set pastetoggle=<F12>
set path=.,/usr/include,,,/usr/include/c++/4.8
set printoptions=paper:letter
set ruler
set runtimepath=~/.vim,~/.vim/bundle/Buffet.vim,~/.vim/bundle/YankRing.vim,~/.vim/bundle/clang_complete,~/.vim/bundle/ctrlp.vim,~/.vim/bundle/tComment,~/.vim/bundle/tagbar,~/.vim/bundle/vim-ass,~/.vim/bundle/vim-colors-solarized,~/.vim/bundle/vim-conque,~/.vim/bundle/vim-fugitive,~/.vim/bundle/vim-hoogle,~/.vim/bundle/vim-powerline,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.vim/bundle/powerline/powerline/bindings/vim
set shortmess=filnxToOat
set showcmd
set showfulltag
set smartindent
set smarttab
set statusline=%f\ %m\ %r\ Line:\ %l/%L[%p%%]\ Col:\ %c\ Buf:\ #%n\ [%b][0x%B]
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set synmaxcol=1024
set tabstop=4
set tags=~/.vim/system-tags,./tags,../tags,tags,TAGS
set ttimeoutlen=50
set updatetime=100
set viminfo='10,<10000,:100,%,s1000,n~/.viminfo
set virtualedit=all
set wildignore=*/.git/*,*/.hg/*,*/.svn/*,*.o,*.so,*.ko,*.bak
set wildmenu
" vim: set ft=vim :
