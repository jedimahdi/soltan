let Hokm = ../package.dhall

let dependencies =
      [{ name = "base", mixin = "hiding (Prelude)" }
      ]

let hokm =
      { source-dirs = "app"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies = [ "hokm-api", "hokm-prelude" ]
      }

let hokm-api =
      { source-dirs = "src"
      , ghc-options = [ "-fplugin=Polysemy.Plugin" ]
      , dependencies =
        [ "attoparsec"
        , "aeson"
        , "async"
        , "chronos"
        , "co-log"
        , "either"
        , "email-validate"
        , "generic-monoid"
        , "http-media"
        , "libjwt-typed"
        , "relude"
        , "scrypt"
        , "selective"
        , "servant-server"
        , "servant-websockets"
        , "validation-selective"
        , "wai"
        , "wai-logger"
        , "wai-cors"
        , "warp"
        , "bytestring"
        , "hokm-prelude"
        , "chronos"
        , "containers"
        , "cookie"
        , "generic-lens"
        , "http-types"
        , "lens"
        , "polysemy"
        , "polysemy-plugin"
        , "postgresql-simple"
        , "servant"
        , "text"
        , "uuid"
        , "websockets"
        , "word8"
        , "random"
        , "random-shuffle"
        , "split"
        , "stm"
        ]
      }

in    Hokm.package
    ⫽ { name = "hokm-api"
      , synopsis = "Hokm Web API"
      , description = "Hokm Web API"
      , category = "Web"
      , dependencies
      , library = hokm-api
      , executables.hokm = hokm
      }
