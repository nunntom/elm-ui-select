#!/usr/bin/env bash

if [ -z "$(git status --porcelain)" ]; then 
  # Working directory clean
    if [ $1 == 'elm-ui' ]
    then
        echo "switching to elm-ui"
        mv src/Select/ElmUi.elm src/Select.elm
        rm src/Select/ElmCss.elm
        rm src/Internal/View/ElmCss.elm
        rm -rfd examples/src/ElmCss
        rm -rfd examples/tests/ElmCss
        rm README.md
        mv README.elm-ui.md README.md
        find . -type f -name "*.elm" -exec sed -i 's/import Select\.ElmUi as Select/import Select/g' {} +
        find . -type f -name "*.elm" -exec sed -i 's/module Select\.ElmUi/module Select/g' {} +
        sed -i ':a;N;$!ba;s/\s\+"Select\.ElmCss",//g' elm.json
        sed -i 's/"Select\.ElmUi",/"Select",/g' elm.json
        npx elm-json uninstall rtfeldman/elm-css --yes
        cd examples && npx elm-json uninstall rtfeldman/elm-css --yes
    elif [ $1 == 'elm-css' ]
        echo "switching to elm-css"
        mv src/Select/ElmCss.elm src/Select.elm
        rm src/Select/ElmUi.elm
        rm src/Internal/View/ElmUi.elm
        rm -rfd examples/src/ElmUi
        rm -rfd examples/tests/ElmUi
        rm README.md
        mv README.elm-css.md README.md
        find . -type f -name "*.elm" -exec sed -i 's/import Select as Select\.ElmCss/import Select/g' {} +
        find . -type f -name "*.elm" -exec sed -i 's/module Select\.ElmCss/module Select/g' {} +
        sed -i ':a;N;$!ba;s/\s\+"Select\.ElmUi",//g' elm.json
        sed -i 's/"Select\.ElmCss",/"Select",/g' elm.json
        npx elm-json uninstall mdgriffith/elm-ui --yes
        cd examples && npx elm-json uninstall mdgriffith/elm-ui --yes
    then
        echo "css"
    else 
        echo "No valid variant specified"
    fi
else 
  echo "There are uncommitted changes!"
fi 