if type --quiet direnv
    eval (direnv hook fish)
end

if type --quiet starship
    eval (starship init fish)
end

if type --quiet hub
    eval (hub alias -s)
end

# for fzf from Nix
if type --quiet fzf-share
    source (fzf-share)/key-bindings.fish
end

set --export LESS '--LONG-PROMPT --RAW-CONTROL-CHARS --quit-if-one-screen --no-init'
