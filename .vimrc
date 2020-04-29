set nonumber

set nocompatible
set term=xterm-256color
set cole=1
syntax on
syntax enable
" filetype off
filetype plugin indent on
set background=dark
runtime macros/matchit.vim

set wildmode=longest,list,full
set wildmenu

hi SpellBad ctermbg=053 guibg=#5f005f
hi SpellCap ctermbg=053 guibg=#5f005f

" Enter in command mode for newline
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>

let loaded_matchparen = 1

set scrolloff=8

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Bundles--

" General
Plugin 'valloric/YouCompleteMe'
Plugin 'scrooloose/syntastic'
let g:ycm_register_as_syntastic_checker = 1
let g:syntastic_check_on_open=1
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exe = '$(npm bin)/eslint'
Plugin 'morhetz/gruvbox'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-vinegar'
" Plugin 'airblade/vim-gitgutter'

" JavaScript
Plugin 'othree/yajs.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'pangloss/vim-javascript'
" Plugin 'othree/javascript-libraries-syntax.vim'
"Plugin 'marijnh/tern_for_vim'
Plugin 'mxw/vim-jsx'
let g:jsx_ext_required = 0
Plugin 'burnettk/vim-angular'
Plugin 'othree/javascript-libraries-syntax.vim' 
let g:used_javascript_libs = 'underscore,angularjs,ramda'
" let g:javascript_conceal = 1
" au BufEnter *.js set conceallevel=2
" au BufEnter *.js syntax keyword jsFunc function conceal cchar=λ

" HTML
Plugin 'tpope/vim-surround'

" SCSS
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'hail2u/vim-css3-syntax'

" Ruby
Plugin 'vim-ruby/vim-ruby'

" Clojure
" Plugin 'guns/vim-clojure-static'
" Plugin 'tpope/vim-fireplace'
" Plugin 'tpope/vim-classpath'
" Plugin 'tpope/vim-leiningen'
" Plugin 'kien/rainbow_parentheses.vim'
" Plugin 'vim-scripts/paredit.vim'
"

" Golang
Plugin 'fatih/vim-go'
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

au BufRead,BufNewFile *.go set filetype=go

call vundle#end()

" set softtabstop=4
" set shiftwidth=4

set tabstop=2
set shiftwidth=2
set expandtab

set nofoldenable
" set foldmethod=indent

filetype plugin indent on

augroup filetype
  au! BufRead,BufNewFile,BufEnter *Makefile*,*makefile*,*.mk set filetype=make
augroup end

autocmd FileType make setlocal noexpandtab

autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1 
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
au BufEnter *.rb set shiftwidth=2 tabstop=2

au BufEnter *.txt set formatoptions-=t

" au VimEnter *.clj RainbowParenthesesToggle
" au Syntax *.clj RainbowParenthesesLoadRound
" au Syntax *.clj RainbowParenthesesLoadSquare
" au Syntax *.clj RainbowParenthesesLoadBraces

imap jj <Esc>
set pastetoggle=<F11>

nnoremap <silent> <C-j> gT 
nnoremap <silent> <C-k> gt

map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

nn <F7> :setlocal spell! spell?<CR>

noremap ff :call RefreshFirefox()<cr>

function! RefreshFirefox()
    silent !xdotool search --onlyvisible firefox key 'F5' &
    redraw!
endfunction

cmap w!! w !sudo tee > /dev/null %

colorscheme Tomorrow-Night-Eighties
nnoremap gf <C-W>gf
