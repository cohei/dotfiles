# Some useful aliases.
alias texclean='rm -f *.toc *.aux *.log *.cp *.fn *.tp *.vr *.pg *.ky'
alias clean='echo -n "Really clean this directory?";
	read yorn;
	if test "$yorn" = "y"; then
	   rm -f \#* *~ .*~ *.bak .*.bak  *.tmp .*.tmp core a.out;
	   echo "Cleaned.";
	else
	   echo "Not cleaned.";
	fi'
alias h='history'
alias j="jobs -l"
alias l="ls -l "
alias ll="ls -l"
alias lla="ls -al"
case $OSTYPE in
    darwin13)  alias ls="ls -FG" ;;
    linux-gnu) alias ls="ls -F --color=auto" ;;
esac

alias pu="pushd"
alias po="popd"

#
# Csh compatability:
#
alias unsetenv=unset
function setenv () {
  export $1="$2"
}

# Function which adds an alias to the current shell and to
# the ~/.bash_aliases file.
add-alias ()
{
   local name=$1 value="$2"
   echo alias $name=\'$value\' >>~/.bash_aliases
   eval alias $name=\'$value\'
   alias $name
}

# "repeat" command.  Like:
#
#	repeat 10 echo foo
repeat ()
{
    local count="$1" i;
    shift;
    for i in $(_seq 1 "$count");
    do
        eval "$@";
    done
}

# Subfunction needed by `repeat'.
_seq ()
{
    local lower upper output;
    lower=$1 upper=$2;

    if [ $lower -ge $upper ]; then return; fi
    while [ $lower -lt $upper ];
    do
	echo -n "$lower "
        lower=$(($lower + 1))
    done
    echo "$lower"
}

# abbreviation for emacs
alias e='emacsclient --alternate-editor="" -n'

alias telnet='rlwrap telnet'

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

alias cd=pushd
alias bd=popd

alias rc='rails console'
