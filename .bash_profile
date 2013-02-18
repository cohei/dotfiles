source ~/.profile

export LANG=ja_JP.UTF-8

export PS1='\W$(__git_ps1) $ '

# export EDITOR=emacs
export EDITOR=emacs-snapshot

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# if [ `ps -A | grep synergys | wc -l` -lt 1 ]
#     then synergys
# fi
