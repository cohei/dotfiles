# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

export HISTSIZE=20000
export HISTFILESIZE=20000

# http://unix.stackexchange.com/questions/18212/bash-history-ignoredups-and-erasedups-setting-conflict-with-common-history/18443#18443
export HISTCONTROL=ignoreboth:erasedups
shopt -s histappend
PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export PS1='\n\[\e[1;37m\][\t] @$(hostname | cut -c 1-6) \W$(__git_ps1_wrapper)\[\e[m\]\n$ '

function __git_ps1_wrapper {
    if command -v __git_ps1 > /dev/null; then
        __git_ps1
    else
        echo -n
    fi
}

# for
#   - git commiting
#   - less v
if [[ $OSTYPE =~ darwin ]]; then
    export EDITOR='emacsclient --alternate-editor="open -a emacs"'
else
    export EDITOR='emacsclient --alternate-editor="" --create-frame'
fi

export LESS='--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
if [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
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

if [ -e ~/.nix-profile/etc/bash_completion.d/git-prompt.sh ]; then
  . ~/.nix-profile/etc/bash_completion.d/git-prompt.sh
fi

if [ -e ~/.nix-profile/etc/profile.d/bash_completion.sh ]; then
    export XDG_DATA_DIRS="$HOME/.nix-profile/share/:$XDG_DATA_DIRS"
    . ~/.nix-profile/etc/profile.d/bash_completion.sh
fi

if [ -e ~/.nix-profile/share/chruby/chruby.sh ]; then
    . ~/.nix-profile/share/chruby/chruby.sh
    . ~/.nix-profile/share/chruby/auto.sh
fi

# for fzf from Nix
if command -v fzf-share > /dev/null; then
    # Auto-completion
    # ---------------
    [[ $- == *i* ]] && source "$(fzf-share)/completion.bash" 2> /dev/null

    # Key bindings
    # ------------
    source "$(fzf-share)/key-bindings.bash"
fi

if type stack >/dev/null 2>&1; then
    eval "$(stack --bash-completion-script "$(which stack)")"
fi

if command -v direnv > /dev/null; then
    eval "$(direnv hook bash)"
fi

if command -v hub > /dev/null; then
    eval "$(hub alias -s)"
fi

shopt -s histverify

function ghq-look {
    declare path
    path=$(ghq list | fzf | xargs ghq list --full-path --exact)

    if [ -n "$path" ]; then
        cd "$path" || exit
    fi
}

# for enhancd from Nix
if command -v enhancd-dir > /dev/null; then
    source "$(enhancd-dir)/init.sh" 2> /dev/null
fi

if [[ "$INSIDE_EMACS" = vterm ]]; then
    export FZF_DEFAULT_OPTS=--bind=ctrl-j:accept
fi

if type brew &>/dev/null; then
    HOMEBREW_PREFIX="$(brew --prefix)"
    if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
        source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
    else
        for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
            [[ -r "$COMPLETION" ]] && source "$COMPLETION"
        done
    fi
fi
