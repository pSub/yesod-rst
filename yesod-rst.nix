{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, directory, hamlet, lib, pandoc, persistent, shakespeare, text
, xss-sanitize, yesod-core, yesod-form
}:
mkDerivation {
  pname = "yesod-rst";
  version = "0.2.3";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html blaze-markup bytestring directory hamlet pandoc
    persistent shakespeare text xss-sanitize yesod-core yesod-form
  ];
  homepage = "http://github.com/pSub/yesod-rst";
  description = "Tools for using reStructuredText (RST) in a yesod application";
  license = "GPL";
}
