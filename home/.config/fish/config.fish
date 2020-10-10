set fish_greeting

set --export LANG ja_JP.UTF-8

set --export PATH ~/.local/bin $PATH

if type --quiet direnv
    eval (direnv hook fish)
end

if type --quiet starship
    eval (starship init fish)
end

set --export LESS '--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init'

# for
#   - git commiting
#   - less v
if test (uname) = "Darwin"
    set --export EDITOR 'emacsclient --alternate-editor="open -a emacs"'
else
    set --export EDITOR 'emacsclient --alternate-editor="" --create-frame'
end

if test -e ~/.nix-profile/share/chruby/chruby.fish
    source ~/.nix-profile/share/chruby/chruby.fish
    source ~/.nix-profile/share/chruby/auto.fish
end

if type --quiet gh
    eval (gh completion --shell fish)
end
