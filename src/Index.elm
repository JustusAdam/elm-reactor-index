module Index where


import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import String
import Util exposing (..)
import Dict exposing (fromList)


-- CONSTANTS


iconPath = "open-iconic/png"
guiDependencySeparator = span [ class "dependency-separator" ] [ text "/" ]
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
  , currpackage   : Maybe Package
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
  , dependencies : List Dependency
  }


-- VIEW


view : Model -> Html
view model =
  let
    packageDependants cpackage =
      [ div
        [ class "right" ]
        [ packageDisplay cpackage
        , dependenciesView cpackage.dependencies
        ]
      ]
  in
    div
      []
      [ pageHeader model
      , div
        [ class "centered page-wrapper" ]
        <| [ folderView model
           ]
           ++ Maybe.withDefault [] (Maybe.map packageDependants model.currpackage)
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
    [ class "dependencies view" ]
    (div [ class "box-header display" ] [ text "Dependencies" ] ::
      List.map dependencyView dependencies)


dependencyView : Dependency -> Html
dependencyView package =
  let
    {account, name, version} = package
  in
    div
      [ class "dependency display element" ]
      [ div
        [ class "dependency-name left" ]
        [ iconBox "left" packageIcon
        , a [ href <| accountUrl package ] [ text account ]
        , guiDependencySeparator
        , a [ href <| packageUrl package ] [ text name ]
        ]
      , div
        [ class "dependency-version right" ]
        [ text version ]
      ]


packageDisplay : Package -> Html
packageDisplay {version, summary, repository} =
  div
    [ class "box view package" ]
    [ div [ class "box-header display" ] [ text "Package Information" ]
    , div [ class "element display" ] [ text summary ]
    , div [ class "element display" ] [ text <| "Package Version: " ++ version ]
    , div [ class "element display" ] [ text repository ]
    ]


-- SIGNALS


main : Signal Html
main = Signal.constant <| view modelPort


port modelPort : Model
