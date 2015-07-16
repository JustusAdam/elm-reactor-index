module Index where


import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import String
import Util exposing (..)
import Dict exposing (fromList)


-- CONSTANTS


iconPath = "open-iconic/png"
guiDependencySeparator = span [ class "package-separator" ] [ text "/" ]
guiPathSeparator = span [ class "path-separator" ] [ text "/" ]
standardIconType = ".png"
standardIconSize = "4x"
fileIcon    = basicIcon "file"
folderIcon  = basicIcon "folder"
packageIcon = basicIcon "book"
htmlIcon    = basicIcon "browser"
imageIcon   = basicIcon "aperture"
codeIcon    = basicIcon "code"
elmIcon     = basicIcon "cog"
textIcon    = basicIcon "justify-left"

endings = fromList
  [ ("elm", elmIcon)
  , ("jpg", imageIcon)
  , ("jpeg", imageIcon)
  , ("png", imageIcon)
  , ("gif", imageIcon)
  , ("html", htmlIcon)
  , ("", fileIcon)
  , ("hs", codeIcon)
  , ("txt", textIcon)
  ]


clearfix : Html
clearfix = div [ class "clearfix" ] []


-- UTILITY FUNCTIONS


packageUrl : Dependency -> String
packageUrl {account, name, version} =
  "http://package.elm-lang.org/packages"
  `slash` account
  `slash` name
  `slash` version


accountUrl : Dependency -> String
accountUrl {account} = "https://github.com" `slash` account


basicIcon name =
  img [ src <| iconPath `slash` name ++ "-" ++ standardIconSize ++ standardIconType, width 14, height 14 ] []


iconBox : String -> Html -> Html
iconBox position icon =
  span
    [ class <| "icon " ++ position ]
    [ icon ]


getIcon : String -> Html
getIcon file =
  Maybe.withDefault fileIcon <| Dict.get (takeExtension file) endings


-- TYPES


type alias Model =
  { currentFolder : String
  , folders       : List String
  , files         : List String
  , dependencies  : List Dependency
  , package       : Maybe Package
  }


type alias Dependency =
  { name    : String
  , account : String
  , version : String
  }


type alias Package =
  { version : String
  , repository : String
  , summary : String
  , license : String
  }


-- VIEW


view : Model -> Html
view model =
  -- This causes a runtime error
  -- > undefined is not an object
  -- > evaluating 'maybe.ctor'
  -- if anyone knows why, please tell me what I'm doing wrong
  -- it compiles fine btw
  -- let
  --   packageView = Maybe.withDefault [] (Maybe.map (\a -> [ packageDisplay a ]) model.package)
  -- in
    div
      []
      [ pageHeader model
      , div
        [ class "centered page-wrapper" ]
        <| [ folderView model
           , dependenciesView model.dependencies
           ]
            -- ++ packageView
      ]



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
   fileClass = if ".elm" `isSuffixOf` file then "elm file" else "file"
  in
    div
      [ class "element display" ]
      <| [ a
        [ class fileClass, href <| file ]
        [ iconBox "left" <| getIcon file, span [ class "file-name" ] [ text file ] ]
      ] ++
        ( if ".elm" `isSuffixOf` file
            then [ a [ class "repl-link", href <| file ++ "?repl" ] [ text "REPL" ]
                 , a [ class "debug-link", href <| file ++ "?debug" ] [ text "Debug" ] ]
            else []
        )


formatSubpathNavigation : String -> List Html
formatSubpathNavigation path =
  let
    subfolderNames = splitPath path
    subFolderPaths = List.drop 1 <| List.scanl (flip slash) "" subfolderNames
    subfolders = List.map2 ((,)) subfolderNames subFolderPaths
  in
    List.map (\(name, path) -> a [ href path ] [ text name ]) subfolders |>
      List.intersperse guiPathSeparator


dependenciesView : List Dependency -> Html
dependenciesView dependencies =
  div
    [ class "packages view right" ]
    (div [ class "box-header display" ] [ text "Packages" ] ::
      List.map dependencyView dependencies)


dependencyView : Dependency -> Html
dependencyView package =
  let
    {account, name, version} = package
  in
    div
      [ class "package display element" ]
      [ div
        [ class "package-name left" ]
        [ iconBox "left" packageIcon
        , a [ href <| accountUrl package ] [ text account ]
        , guiDependencySeparator
        , a [ href <| packageUrl package ] [ text name ]
        ]
      , div
        [ class "package-version right" ]
        [ text version ]
      ]


packageDisplay : Package -> Html
packageDisplay {version, summary, repository} =
  div
    [ class "box" ]
    [ div [ class "box-item" ] [ text <| "Package Version: " ++ version ]
    , div [ class "box-item" ] [ text summary ]
    , div [ class "box-item" ] [ text repository ]
    ]


-- SIGNALS


main : Signal Html
main = Signal.constant <| view modelPort


port modelPort : Model
