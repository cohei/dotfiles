function gh-ci-status
    set --local revision (git rev-parse $argv[1])
    set --local header 'Accept: application/vnd.github.antiope-preview+json'

    gh api --header $header repos/:owner/:repo/commits/{$revision}/check-runs | \
        jq --raw-output '.check_runs[] | "\(.conclusion)\t\(.name)\t\(.html_url)"'
end
