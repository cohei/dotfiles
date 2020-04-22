function ghq-look
    set --local path (ghq list | fzf | xargs ghq list --full-path --exact)

    if test -z $path
        return
    end

    cd $path
end
