# PATH Config
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/Users/ryosuke/my/py34/bin:$PATH"
export PATH="/Users/ryosuke/.cask/bin:$PATH"

# language environment
export LANG=ja_JP.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# ssh
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
 
# emacs 風キーバインドにする
bindkey -e
 
# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
 
# プロンプト
# 2行表示
PROMPT="%{${fg[red]}%}[%n@%m]%{${reset_color}%} %~
%# "
RPROMPT="%*"

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

# Alias
alias ls='ls -a'
alias ll='ls -l'
 
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
 
alias mkdir='mkdir -p'
