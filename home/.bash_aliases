# Function which adds an alias to the current shell and to
# the ~/.bash_aliases file.
add-alias ()
{
   local name=$1 value="$2"
   echo alias "$name"=\'"$value"\' >>~/.bash_aliases
   eval alias "$name"=\'"$value"\'
   alias "$name"
}

# "repeat" command.  Like:
#
#	repeat 10 echo foo
repeat ()
{
    local count="$1";
    shift;
    for _ in $(seq 1 "$count");
    do
        eval "$@";
    done
}

case $OSTYPE in
    darwin*)   alias ls="ls -FG" ;;
    linux-gnu) alias ls="ls -F --color=auto" ;;
esac

alias la="ls -a"
alias ll="ls -l"
alias lla="ls -al"

# abbreviation for emacs
alias e='emacsclient --alternate-editor="" -n'

# git
alias g='git'
alias s='g s'
alias d='g diff'
alias glg='g lg'
alias gl='g l'
alias gdc='g dc'
alias gap='g ap'
alias gsp='g sp'
alias gg='g g'

alias dfh='df -h'

# For solarize colored terminals
alias icdiff="icdiff --no-bold"
