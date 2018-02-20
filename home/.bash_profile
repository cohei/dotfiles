export HISTSIZE=20000
export HISTFILESIZE=20000

export LANG=ja_JP.UTF-8

export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export PS1='\n[\t] @$(hostname | cut -c 1-6) \W$(__git_ps1) $ '

# mainly for git commiting
if [ "$(uname)" = 'Linux' ]; then
    export EDITOR='emacsclient --alternate-editor="" -c'
else
    export EDITOR='emacsclient --alternate-editor=""'
fi

export PATH=$HOME/bin:$PATH
export PATH=$HOME/.local/bin:$PATH

export LESS='--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init'

# shellcheck source=.bashrc
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
