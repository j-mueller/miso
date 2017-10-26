{ stdenv, fetchurl, makeWrapper, jre, jdk, gcc, xorg
, htmlunit-driver, chromedriver, chromeSupport ? true }:

with stdenv.lib;

let
  minorVersion = "2.53";
  patchVersion = "1";
  arch = if stdenv.system == "x86_64-linux" then "amd64"
         else if stdenv.system == "i686-linux" then "i386"
         else "";

in stdenv.mkDerivation rec {
  name = "selenium-server-standalone-${version}";
  version = "${minorVersion}.${patchVersion}";

  src = fetchurl {
    url = "http://selenium-release.storage.googleapis.com/${minorVersion}/selenium-server-standalone-${version}.jar";
    sha256 = "1y3w1c2173vn2yqy6047l6lxmg919xyi19ccw4my7cm5bhx6vkhw";
  };

  unpackPhase = "true";

  buildInputs = [ jre makeWrapper ];

  installPhase = ''
    mkdir -p $out/share/lib/${name}
    cp $src $out/share/lib/${name}/${name}.jar
    makeWrapper ${jre}/bin/java $out/bin/selenium-server \
      --add-flags "-cp $out/share/lib/${name}/${name}.jar:${htmlunit-driver}/share/lib/${htmlunit-driver.name}/${htmlunit-driver.name}.jar" \
      --add-flags ${optionalString chromeSupport "-Dwebdriver.chrome.driver=${chromedriver}/bin/chromedriver"} \
      --add-flags "org.openqa.grid.selenium.GridLauncher"
  '';

  meta = {
    homepage = http://www.seleniumhq.org/;
    description = "Selenium Server for remote WebDriver";
    maintainers = with maintainers; [ coconnor offline ];
    platforms = platforms.all;
    license = licenses.asl20;
  };
}
