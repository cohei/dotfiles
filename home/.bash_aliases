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
    darwin*)   alias ls='ls --classify --no-group --color' ;;
    linux-gnu) alias ls='ls -F --color=auto' ;;
esac

alias la='ls -A'
alias ll='ls -A -l --human-readable'

alias dfh='df --human-readable'
alias ..='cd ..'

# For solarize colored terminals
alias icdiff='icdiff --no-bold'
