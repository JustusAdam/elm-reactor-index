(function () {
  document.initialModel = {
    currentFolder: "test/foo/bar",
    folders: [
      "one",
      "two",
      "three"
    ],
    files: [
      "answer42.txt",
      "Index.elm",
      "String.elm",
      "Conquer.hs",
      "photo.jpg",
      "native.js",
      ".gitignore",
      "elm-package.json",
      "bower.json",
      "some-data.xml",
      "index.html",
      "LICENSE",
      "data.json",
      "Gemfile.lock",
      "music.ogg",
      "production.db",
      "server.log",
      "build.sh"
    ],
    dependencies: [
      {
        name : "elm-html",
        account : "evancz",
        version : "1.0.0"
      },
      {
        name : "elm-core",
        account : "elm-lang",
        version : "2.1.0"
      }
    ],
    package: {
      version: "1.0.0",
      repository: "https://github.com/foo/bar",
      license: "BSD3",
      summary: "A new package"
    }
  }
})();
