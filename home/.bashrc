# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# http://unix.stackexchange.com/questions/18212/bash-history-ignoredups-and-erasedups-setting-conflict-with-common-history/18443#18443
export HISTCONTROL=ignoreboth:erasedups
shopt -s histappend
PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
if [[ ! $OSTYPE =~ darwin ]]; then
    shopt -s globstar
fi

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Alias definitions.
# shellcheck source=.bash_aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -e "$HOME/.nix-profile/etc/bash_completion.d" ]; then
  . "$HOME/.nix-profile/etc/bash_completion.d/git-completion.bash"
  . "$HOME/.nix-profile/etc/bash_completion.d/git-prompt.sh"
fi

if type rbenv >/dev/null 2>&1; then
    eval "$(rbenv init -)"
fi

if type stack >/dev/null 2>&1; then
    eval "$(stack --bash-completion-script "$(which stack)")"
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

ENHANCD_DIR=~/src/github.com/b4b4r07/enhancd
[ -f $ENHANCD_DIR/init.sh ] && . $ENHANCD_DIR/init.sh

# for `ghq look` not to use subshell
ghq () {
    if [ "$1" = look ] && [ -n "$2" ]; then
        cd "$(command ghq list -p -e "$2")" || exit
    else
        command ghq "$@"
    fi
}

eval "$(direnv hook bash)"

eval "$(hub alias -s)"
