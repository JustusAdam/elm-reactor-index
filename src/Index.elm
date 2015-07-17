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
standardIconDimensions = 14
homeIcon size = sizedIcon size "home"
homeIconLarge = homeIcon 18
homeIconSmall = homeIcon standardIconDimensions
fileIcon      = basicIcon "file"
folderIcon    = basicIcon "folder"
packageIcon   = basicIcon "book"
htmlIcon      = basicIcon "browser"
imageIcon     = basicIcon "aperture"
codeIcon      = basicIcon "code"
cogIcon       = basicIcon "cog"
wrenchIcon    = basicIcon "wrench"
configIcon    = wrenchIcon
textIcon      = basicIcon "document"
dataIcon      = basicIcon "spreadsheet"
keyIcon       = basicIcon "key"
legalIcon     = keyIcon
lockIcon      = basicIcon "lock-locked"
audioIcon     = basicIcon "audio"
listIcon      = basicIcon "list"
hardDriveIcon = basicIcon "hard-drive"
dataDaseIcon  = hardDriveIcon
terminalIcon  = basicIcon "terminal"


endings = fromList
  [
  -- Code
    ("elm", codeIcon)
  , ("hs", codeIcon)
  , ("lhs", codeIcon)
  , ("js", codeIcon)
  , ("php", codeIcon)
  , ("py", codeIcon)
  , ("rb", codeIcon)
  , ("coffee", codeIcon)
  , ("sh", terminalIcon)
  , ("bash", terminalIcon)

  -- Images
  , ("jpg", imageIcon)
  , ("jpeg", imageIcon)
  , ("png", imageIcon)
  , ("gif", imageIcon)

  -- Audio
  , ("mp3", audioIcon)
  , ("wma", audioIcon)
  , ("ogg", audioIcon)
  , ("oga", audioIcon)
  , ("flac", audioIcon)

  -- Data formats
  , ("json", dataIcon)
  , ("yml", dataIcon)
  , ("xml", dataIcon)
  , ("db", dataDaseIcon)
  , ("sqlite", dataDaseIcon)

  -- Text files
  , ("txt", textIcon)
  , ("log", listIcon)

  -- Misc
  , ("md", textIcon)
  , ("", fileIcon)
  , ("html", htmlIcon)
  , ("htm", htmlIcon)
  , ("lock", lockIcon)
  ]


fullFileIcons = fromList
  [ (".gitignore", configIcon)
  , ("elm-package.json", configIcon)
  , ("bower.json", configIcon)
  , ("license", legalIcon)
  , ("license.txt", legalIcon)
  , ("makefile", configIcon)
  ]


clearfix : Html
clearfix = div [ class "clearfix" ] []


-- UTILITY FUNCTIONS


packageUrl : Dependency -> String
packageUrl {account, name, version} =
  "http://package.elm-lang.org/packages"
  </> account
  </> name
  </> version


accountUrl : Dependency -> String
accountUrl {account} = "https://github.com" </> account


basicIcon = sizedIcon standardIconDimensions


sizedIcon size name =
  img [ src <| iconPath </> name ++ "-" ++ standardIconSize ++ standardIconType, width size, height size ] []


iconBox : String -> Html -> Html
iconBox position icon =
  span
    [ class <| "icon " ++ position ]
    [ icon ]


getIcon : String -> Html
getIcon filename =
  let file = String.toLower filename in
  Maybe.withDefault fileIcon <| Dict.get file fullFileIcons `or` Dict.get (takeExtension file) endings


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
           ++ [ clearfix ]
      ]



pageHeader : Model -> Html
pageHeader {currentFolder} =
  header
    []
    [ div
      [ class "header-wrapper" ]
      [ div [ class "current-folder left" ] <| formatSubpathNavigation  homeIconSmall currentFolder
      , clearfix
      ]
    ]


folderView : Model -> Html
folderView {currentFolder, folders, files} =
  section
    [ class "folder-navigation" ]
    [ h2 [] <| formatSubpathNavigation homeIconLarge currentFolder
    , div
      [ class "folder view left" ]
      (div [ class "box-header display" ] [ text "File Navigation" ] ::
        -- This for some reason does not properly sort alphabetically ...
        List.map folderDisplay (List.sort folders) ++
          List.map fileDisplay (List.sort files)
      )
    ]


folderDisplay : String -> Html
folderDisplay folder =
  a [ class "folder element display", href <| folder ] [ iconBox "left" folderIcon, text folder ]


fileDisplay : String -> Html
fileDisplay file =
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


formatSubpathNavigation : Html -> String -> List Html
formatSubpathNavigation home path =
  let
    subfolderNames = splitPath path
    subFolderPaths = List.drop 1 <| List.scanl (flip (</>)) "" <| "" :: subfolderNames
    subfolderNameRepresentation = List.map text subfolderNames
    subfolders = (home :: subfolderNameRepresentation) >< subFolderPaths
  in
    List.map (\(name, path) -> a [ href path ] [ name ]) subfolders |>
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
