{ ... }:

{
  homebrew = {
    casks = [
      "aquaskk@prerelease" # for M3 MacBook compatibility
      "claude"
      { name = "docker-desktop"; greedy = true; }
      { name = "google-drive"; greedy = true; }
      { name = "karabiner-elements"; greedy = true; }
      "macfuse" # for tup
      "michaelvillar-timer"
    ];

    masApps = {
      "Control Panel for Twitter" = 1668516167;
      "Keepa - Price Tracker" = 1533805339;
      "Paste Plain Text" = 1407015686;
      "Refined GitHub" = 1519867270;
      "Save to Raindrop.io" = 1549370672;
      "Slack" = 803453959;
      "Spark Desktop" = 6445813049;
      "The Unarchiver" = 425424353;
      "Things" = 904280696;
      "URL Linker" = 1289119450;
      "Vimari" = 1480933944;

      # Apple preinstalled apps, listed only so `brew bundle cleanup` keeps them
      "Keynote" = 361285480;
      "Numbers" = 361304891;
      "Pages" = 361309726;
    };
  };
}
