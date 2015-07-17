# elm-reactor-index

A new index page/folder navigation for [elm-reactor](https://github.com/elm-lang/elm-reactor)

## Current status

![phantom.js screenshot rendering](/screenshots/elm-reactor-index-1280.png?raw=true "Current Screenshot")

## Testing

Needs a [sass](http://sass-lang.org) compiler, such as [compass](http://compass-style.org), to compile the stylesheets.

Easiest way of installing is trough ruby gems. Make sure you have ruby installed, and execute `gem install compass` on the command line. Use `compass compile` in the projects root directory to compile the stylesheets and if you are developing style `compass watch` to automatically rebuild if the .scss files are modified.

Finally use `elm make src/Index.elm` to compile the elm code.

Open index.html.

## Disclaimer

Links to individual files will obviously not work, the backend server is not present.
