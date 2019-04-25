# Function which adds an alias to the current shell and to
# the ~/.bash_aliases file.
add-alias ()
{
   local name=$1 value=$2
   echo alias "$name"=\'"$value"\' >> ~/.bash_aliases
   eval alias "$name"=\'"$value"\'
   alias "$name"
}

# "repeat" command.  Like:
#
# repeat 10 echo foo
repeat ()
{
    local count=$1;
    shift;
    for _ in $(seq 1 "$count");
    do
        eval "$@";
    done
}

case $OSTYPE in
    darwin*)   alias ls='ls -FG' ;;
    linux-gnu) alias ls='ls -F --color=auto' ;;
esac

alias la='ls -a'
alias ll='ls -l'
alias lla='ls -al'

if [[ $OSTYPE =~ darwin ]]; then
    alias e='emacsclient --tty --no-wait --alternate-editor="open -a emacs"'
else
    alias e='emacsclient --alternate-editor="" --create-frame'
fi

# git
alias d='git d'
alias gap='git ap'
alias gb='git co $(git branch | fzf | tr -d "* ")'
alias gdc='git dc'
alias gg='git g'
alias gl='git l'
alias glg='git lg'
alias gsl='git sl'
alias gsp='git sp'
alias gss='git ss'
alias s='git s'

alias dfh='df -h'
alias ..='cd ..'

# For solarize colored terminals
alias icdiff='icdiff --no-bold'
