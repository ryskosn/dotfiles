# PATH
set -x PATH /opt/local/bin /opt/local/sbin $PATH
set -x PATH $HOME/bin $HOME/.cask/bin $HOME/py35/bin $PATH

# Go
set -x GOROOT (go env GOROOT)
set -x GOPATH $HOME/go
set -x PATH $GOPATH/bin $GOROOT/bin $PATH

# nodebrew
set -x PATH $HOME/.nodebrew/current/bin $PATH

# Heroku Toolbelt
set -x PATH /usr/local/heroku/bin $PATH


# alias
alias ls "ls -aG"
alias rm "rm -i"
alias cp "cp -i"
alias mv "mv -i"
alias mkdir "mkdir -p"

alias find "gfind"
alias xargs "gxargs"
alias ec "emacsclient -nw"

# alias for git
alias gst "git status"
alias gdif "git diff"
alias gdifc "git diff --cached"

# alias for Mac
alias ql "qlmanage -p $argv[1]"

# user defined functions
function cd
  builtin cd $argv
  ls
end

function fish_user_key_bindings
  bind \cr peco_select_history
  bind \co peco_select_ghq_repository
end
