{ pkgs, ... }:

{
  home.packages = [ pkgs.ghq ];

  programs.fish = {
    # fish loads only the first ghq.fish in $fish_complete_path, so source ghq's own.
    completions.ghq = ''
      source ${pkgs.ghq}/share/fish/vendor_completions.d/ghq.fish

      functions --copy __fish_ghq_needs_subcommand __fish_ghq_needs_subcommand_builtin
      function __fish_ghq_needs_subcommand
          __fish_ghq_needs_subcommand_builtin; and not __fish_seen_subcommand_from look
      end
      complete --command ghq --condition __fish_ghq_needs_subcommand --arguments look --description 'Pick a repository with fzf and cd into it'
    '';

    functions.ghq = ''
      function __ghq_look
          set --local repo (command ghq list | fzf)

          if test -z "$repo"
              return 0
          end

          cd (command ghq list --full-path --exact $repo)
      end

      if test "$argv[1]" = look
          __ghq_look
      else
          command ghq $argv
      end
    '';
  };
}
