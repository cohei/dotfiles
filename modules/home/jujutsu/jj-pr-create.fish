if test (count $argv) -lt 1
    echo 'usage: jj-pr-create <revision> [gh pr create flags]' >&2
    return 1
end

set --local revision $argv[1]

jj git push --change $revision
or return

set --local bookmark (jj log --revisions $revision --no-graph --template (jj config get templates.git_push_bookmark))
or return

set --local title (jj log --revisions $revision --no-graph --template 'description.first_line()')
or return

gh pr create --draft --head $bookmark --title $title $argv[2..]
or return

if isatty stdout
    gh pr view --web $bookmark
end
