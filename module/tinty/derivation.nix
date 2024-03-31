{ fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "tinty";
  version = "0.11.0";

  src = fetchFromGitHub {
    owner = "tinted-theming";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-wUOnhgTnyLqkcc1+nE8fTUTkVj8yhsSN4JUFpW5JLAk=";
  };

  cargoHash = "sha256-fvCyMvtrk4c0E5OF0AC394kCUSWs9SlHGwmO8i3VrbY=";

  # Tests faild, maybe due to homeless-shelter
  doCheck = false;
}
