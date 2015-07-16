module Index where


import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import String
import Util exposing (..)


-- CONSTANTS


iconPath = "vendor/open-iconic-master/png"
guiPackageSeparator = span [ class "package-separator" ] [ text "/" ]
guiPathSeparator = span [ class "path-separator" ] [ text "/" ]
fileIcon = basicIcon "file-4x.png"
folderIcon = basicIcon "folder-4x.png"
packageIcon = basicIcon "book-4x.png"

clearfix : Html
clearfix = div [ class "clearfix" ] []


-- UTILITY FUNCTIONS


packageUrl : Package -> String
packageUrl {account, name, version} = "http://package.elm-lang.org/packages" `slash` account `slash` name `slash` version


accountUrl : Package -> String
accountUrl {account} = "https://github.com" `slash` account


basicIcon name =
  img [ src <| iconPath `slash` name, width 12, height 12 ] []


iconBox : String -> Html -> Html
iconBox position icon =
  span
    [ class <| "icon " ++ position ]
    [ icon ]


-- TYPES


type alias Model =
  { currentFolder : String
  , folders  : List String
  , files    : List String
  , packages : List Package
  }


type alias Package =
  { name : String
  , account : String
  , version : String
  }


-- VIEW


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


pageHeader : Model -> Html
pageHeader {currentFolder} =
  header
    []
    [ div
      [ class "header-wrapper" ]
      [ div [ class "current-folder left" ] <| formatSubpathNavigation currentFolder
      , clearfix
      ]
    ]


folderView : Model -> Html
folderView {currentFolder, folders, files} =
  section
    [ class "folder-navigation" ]
    [ h2 [] <| formatSubpathNavigation currentFolder
    , div
      [ class "folder view left" ]
      (div [ class "box-header display" ] [ text "File Navigation" ] ::
        List.map (folderDisplay currentFolder) folders ++
          List.map (fileDisplay currentFolder) files
      )
    ]


folderDisplay : String -> String -> Html
folderDisplay basefolder folder =
  a [ class "folder element display", href <| folder ] [ iconBox "left" folderIcon, text folder ]


fileDisplay : String -> String -> Html
fileDisplay basefolder file =
  let
   fileClass = if ".elm" `isSuffixOf` file then "elm file-name" else "file-name"
  in
    div
      [ class "element display" ]
      <| [ a
        [ class "file", href <| file ]
        [ iconBox "left" fileIcon, span [ class fileClass ] [ text file ] ]
      ] ++
        ( if ".elm" `isSuffixOf` file
            then [ a [ class "repl-link" ] [ text "REPL" ]
                 , a [ class "debug-link", href <| file ++ "?debug" ] [ text "Debug" ] ]
            else []
        )


formatSubpathNavigation : String -> List Html
formatSubpathNavigation path =
  let
    subfolderNames = String.split "/" path
    subFolderPaths = List.drop 1 <| List.scanl (flip slash) "" subfolderNames
    subfolders = List.map2 ((,)) subfolderNames subFolderPaths
  in
    List.intersperse guiPathSeparator <|
      List.map (\(name, path) -> a [ href path ] [ text name ]) subfolders


packagesView : List Package -> Html
packagesView packages =
  div
    [ class "packages view right" ]
    (div [ class "box-header display" ] [ text "Packages" ] ::
      List.map packageDisplay packages)


packageDisplay : Package -> Html
packageDisplay package =
  let
    {account, name, version} = package
  in
    div
      [ class "package display element" ]
      [ div
        [ class "package-name left" ]
        [ iconBox "left" packageIcon
        , a [ href <| accountUrl package ] [ text account ]
        , guiPackageSeparator
        , a [ href <| packageUrl package ] [ text name ]
        ]
      , div
        [ class "package-version right" ]
        [ text version ]
      ]


-- SIGNALS


main : Signal Html
main = Signal.constant <| view modelPort


port modelPort : Maybe Model