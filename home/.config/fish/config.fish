if type --quiet direnv
    eval (direnv hook fish)
end

if type --quiet starship
    eval (starship init fish)
end
