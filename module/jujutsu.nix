{ ... }:

{
  programs.jujutsu = {
    enable = true;
    settings = {
      colors.rest = "black";
      aliases.s = ["status"];
      user = {
        email = "a.d.xvii.kal.mai@gmail.com";
        name = "TANIGUCHI Kohei";
      };
    };
  };

  programs.fish = {
    shellAbbrs = {
      j = "jj";
    };
  };
}
