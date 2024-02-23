{ fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "tinty";
  version = "0.10.1";

  src = fetchFromGitHub {
    owner = "tinted-theming";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-7E/ApLoQLD+Gi9UqJDuSsx1p1aGyjxIH/CKfmnuwlsk=";
  };

  cargoHash = "sha256-XuB8wDscn6eJLHQxPPhv9GaKpymK19rkKUCP1bSNma0=";

  # Tests faild, maybe due to homeless-shelter
  doCheck = false;
}
