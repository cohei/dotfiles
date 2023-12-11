function gh-ci-status
    argparse 'open' -- $argv
    or return

    set --local revision (git rev-parse $argv[1])
    set --local result (gh api repos/:owner/:repo/commits/{$revision}/check-runs --jq '.check_runs[] | "\(.conclusion)\t\(.name)\t\(.html_url)"')

    if set --query _flag_open
        open (string split -f 3 \t $result)
    else
        echo $result
    end
end
