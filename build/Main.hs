import Development.Shake
    ( Rules, CmdOption(Cwd, Shell), (%>)
    , phony, cmd, need, shakeArgs, shakeOptions, want
    )
import Development.Shake.FilePath ((<.>), (</>))

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["cabal-files"]

    sequence_ $ formatRoot <$> dhallRootImports
    sequence_ $ formatPkg <$> dhallPkgs
    sequence_ $ hpack <$> dhallPkgs
    sequence_ $ cabal <$> dhallCabal
    phony "dhall-format" $ need $ ("dhall-format-" ++) <$> dhallPkgs ++ dhallRootImports
    phony "hpack-dhall" $ need $ ("hpack-dhall-" ++) <$> dhallPkgs
    phony "cabal-files" $ need $ (\(x, y) -> x </> y <.> "cabal") <$> dhallCabal

type Folder = String
type Pkg = String

-- | The names of the folders for dhall-format and hpack-dhall.
dhallPkgs :: [Folder]
dhallPkgs = fst <$> dhallCabal

-- | Pairs of package folder name used by dhall and the produced cabal file
-- name.
dhallCabal :: [(Folder, Pkg)]
dhallCabal =
    [ ("uom-plugin", "uom-plugin")
    , ("uom-plugin-defs", "uom-plugin-defs")
    , ("uom-plugin-examples", "uom-plugin-examples")
    , ("uom-plugin-tutorial", "uom-plugin-tutorial")
    , ("build", "build-uom-plugin")
    ]

dhallRootImports :: [String]
dhallRootImports = ["defaults"]

formatPkg :: Folder -> Rules ()
formatPkg folder =
    phony ("dhall-format-" ++ folder)
    $ cmd Shell ("dhall format " ++ (folder </> "package.dhall"))

formatRoot :: String -> Rules ()
formatRoot x =
    phony ("dhall-format-" ++ x)
    $ cmd Shell ("dhall format " ++ (x <.> ".dhall"))

hpack :: Folder -> Rules ()
hpack folder =
    phony ("hpack-dhall-" ++ folder) $ do
        need ["dhall-format-" ++ folder]
        cmd (Cwd folder) Shell "dhall-hpack-cabal --package-dhall=package.dhall"

cabal :: (Folder, Pkg) -> Rules ()
cabal (folder, pkg) =
    folder </> pkg <.> "cabal" %> \_ -> need ["hpack-dhall-" ++ folder]
