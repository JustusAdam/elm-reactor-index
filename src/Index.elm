module Index where


import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import String exposing (endsWith)


type alias Model =
  { currentFolder : String
  , folders  : List String
  , files    : List String
  , packages : List Package
  }


type alias Package =
  { name : String
  }


iconPath = "vendor/open-iconic-master/png"


basicIcon name =
  img [ src <| iconPath `slash` name, width 12, height 12 ] []


fileIcon = basicIcon "file-4x.png"
folderIcon = basicIcon "folder-4x.png"
packageIcon = basicIcon "book-4x.png"


iconBox : String -> Html -> Html
iconBox position icon =
  span
    [ class <| "icon " ++ position ]
    [ icon ]


slash : String -> String -> String
slash a b =
  if endsWith "/" a
    then a ++ b
    else a ++ "/" ++ b


view : Maybe Model -> Html
view model =
  let
    modelMissing = div [ class "alert" ] [ text "no folder was found" ]
    view' model =
      div
        []
        [ pageHeader model
        , div
          [ class "centered page-wrapper" ]
          [ folderView model
          , packagesView model.packages
          ]
        ]
  in
    model |> Maybe.map view' |> Maybe.withDefault modelMissing


clearfix : Html
clearfix = div [ class "clearfix" ] []


pageHeader : Model -> Html
pageHeader {currentFolder} =
  header
    []
    [ div [ class "current-folder left" ] [ text currentFolder ]
    , clearfix
    ]

folderView : Model -> Html
folderView {currentFolder, folders, files} =
  div
    [ class "folder view left" ]
    (div [ class "box-header display" ] [ text "File Navigation" ] ::
      List.map (folderDisplay currentFolder) folders ++
        List.map (fileDisplay currentFolder) files
    )


folderDisplay : String -> String -> Html
folderDisplay basefolder folder =
  a [ class "folder element display", href <| folder ] [ iconBox "left" folderIcon, text folder ]


fileDisplay : String -> String -> Html
fileDisplay basefolder file =
  let
   fileClass = if endsWith ".elm" file then "elm file-name" else "file-name"
  in
    div
      [ class "element display" ]
      <| [ a
        [ class "file", href <| file ]
        [ iconBox "left" fileIcon, span [ class fileClass ] [ text file ] ]
      ] ++
        ( if endsWith ".elm" file
            then [ a [ class "repl-link" ] [ text "REPL" ], a [ class "debug-link" ] [ text "Debug" ] ]
            else []
        )



packagesView : List Package -> Html
packagesView packages =
  div
    [ class "packages view right" ]
    (div [ class "box-header display" ] [ text "Packages" ] ::
      List.map packageDisplay packages)


packageDisplay : Package -> Html
packageDisplay {name} = div [ class "package display element" ] [ iconBox "left" packageIcon, text name ]


main : Signal Html
main = Signal.constant <| view modelPort


port modelPort : Maybe Model
