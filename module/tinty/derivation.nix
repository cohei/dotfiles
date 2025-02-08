{ fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "tinty";
  version = "0.26.0";

  src = fetchFromGitHub {
    owner = "tinted-theming";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-tQW8z0Gtxh0cnMwm9oN3PyOQW7YFVXG2LDkljudMDp0==";
  };

  cargoHash = "sha256-R4mY/jo8uP0aPQy2+u2vtjibRMNJrWvgbCH4kptrO4U=";

  # Tests faild, maybe due to homeless-shelter
  doCheck = false;
}
