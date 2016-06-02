# PATH Settings
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.cask/bin:$PATH
export PATH=$HOME/py34/bin:$PATH

# GO
export GO15VENDOREXPERIMENT=1
export GOROOT=`go env GOROOT`
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH

# rbenv
[[ -d ~/.rbenv  ]] && \
  export PATH=${HOME}/.rbenv/bin:${PATH} && \
  eval "$(rbenv init -)"

### added by the Heroku Toolbelt
export PATH=/usr/local/heroku/bin:$PATH

# nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

# F#
export DYLD_FALLBACK_LIBRARY_PATH="${HOME}/lib:/usr/local/lib:/opt/local/lib:/lib:/usr/lib:${DYLD_FALLBACK_LIBRARY_PATH}$"


# language environment
export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8



### ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# https://gist.github.com/mollifier/4979906

# 補完機能を有効にする
autoload -Uz compinit
compinit

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 色を使用出来るようにする
autoload -Uz colors
colors

# LS_COLORS
# http://qiita.com/yuyuchu3333/items/84fa4e051c3325098be3
# eval $(gdircolors $HOME/src/dircolors-solarized/dircolors.ansi-universal)

if [ -n "$LS_COLORS" ]; then
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

# emacs 風キーバインドにする
bindkey -e

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

# プロンプト
# 2行表示
PROMPT="%{${fg[red]}%}[%n@%m]%{${reset_color}%} %~
%* %# "
# RPROMPT="%*"

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# cd したら自動的にpushdする
setopt auto_pushd

# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# ヒストリファイルに保存するとき、すでに重複したコマンドがあったら古い方を削除する
setopt hist_save_nodups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# 補完候補が複数あるときに自動的に一覧表示する
setopt auto_menu

# 高機能なワイルドカード展開を使用する
setopt extended_glob

### Alias
# coreutils をインストールする
alias ls='gls -a --color=auto'
alias ll='gls -l --color=auto'
alias lls='ls -a'

# findutils をインストールする
alias find='gfind'
alias xargs='gxargs'

# add option
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'
alias grep='grep --color=auto'

# Git
alias gst='git status'

# open, quicklook
function op() {
    if [ -z "$1" ]; then
        open .
    else
        open "$@"
    fi
}
alias ql='qlmanage -p "$@" >& /dev/null'


### PostgreSQL
# http://succzero.hatenablog.com/entry/2014/09/21/133315
export PGDATA=/opt/local/var/db/postgresql94/defaultdb

# OPAM configuration
. /Users/ryosuke/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true


### peco
# http://qiita.com/shepabashi/items/f2bc2be37a31df49bca5

function peco-history-selection() {
    BUFFER=`history -n 1 | tail -r  | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N peco-history-selection
bindkey '^R' peco-history-selection

# http://r7kamura.github.io/2014/06/21/ghq.html
function p() {
    peco | while read LINE; do $@ $LINE; done
}
alias e='ghq list -p | p cd'

alias peco-pushd="pushd +\$(dirs -p -v -l | sort -k 2 -k 1n | uniq -f 1 | sort -n | peco | head -n 1 | awk '{print \$1}')"

# http://futurismo.biz/archives/2514
function peco-find-file () {
    ls | peco | xargs emacsclient -nw
    zle clear-screen
}
zle -N peco-find-file
bindkey '^x^f' peco-find-file # C-x C-f
