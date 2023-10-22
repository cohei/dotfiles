function gh-ci-status
    argparse 'open' -- $argv
    or return

    set --local revision (git rev-parse $argv[1])
    set --local header 'Accept: application/vnd.github.antiope-preview+json'
    set --local json (gh api --header $header repos/:owner/:repo/commits/{$revision}/check-runs)
    set --local result (echo $json | jq --raw-output '.check_runs[] | "\(.conclusion)\t\(.name)\t\(.html_url)"')

    if set --query _flag_open
        open (string split -f 3 \t $result)
    else
        echo $result
    end
end
