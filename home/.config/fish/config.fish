set fish_greeting

if type --quiet direnv
    direnv hook fish | source
end

if type --quiet starship
    starship init fish | source
end

if test -e ~/.nix-profile/share/chruby/chruby.fish
    source ~/.nix-profile/share/chruby/chruby.fish
    source ~/.nix-profile/share/chruby/auto.fish
end

if type --quiet gh
    eval (gh completion --shell fish)
end
