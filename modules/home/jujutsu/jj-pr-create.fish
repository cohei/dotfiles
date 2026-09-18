if test (count $argv) -ne 1
    echo 'usage: jj-pr-create <revision>' >&2
    return 1
end

set --local revision $argv[1]

jj git push --change $revision
or return

set --local bookmark (jj log --revisions $revision --no-graph --template (jj config get templates.git_push_bookmark))
or return

gh pr create --draft --head $bookmark
or return

gh pr view --web $bookmark
