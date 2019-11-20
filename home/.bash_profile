export HISTSIZE=20000
export HISTFILESIZE=20000

export LANG=ja_JP.UTF-8

export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export PS1='\n[\t] @$(hostname | cut -c 1-6) \W$(__git_ps1) $ '

# for
#   - git commiting
#   - less v
if [[ $OSTYPE =~ darwin ]]; then
    export EDITOR='emacsclient --alternate-editor="open -a emacs"'
else
    export EDITOR='emacsclient --alternate-editor="" --create-frame'
fi

export PATH=~/.local/bin:$PATH

export LESS='--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init'

# shellcheck source=.bashrc
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

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
