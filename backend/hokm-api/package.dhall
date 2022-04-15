let Hokm = ../package.dhall

let dependencies =
      [{ name = "base", mixin = "hiding (Prelude)" }
      ]

let hokm =
      { source-dirs = "app"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies = [ "hokm-api", "hokm-prelude", "postgresql-simple" ]
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

let hokm-api-test =
      { source-dirs = "test"
      , main = "Main.hs"
      , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
      , dependencies =
        [ "binary"
        , "hokm-api"
        , "hokm-prelude"
        , "directory"
        , "filepath"
        , "free"
        , "hedgehog"
        , "http-client"
        , "http-types"
        , "lifted-async"
        , "lifted-base"
        , "managed"
        , "monad-control"
        , "port-utils"
        , "postgres-options"
        , "relude"
        , "resource-pool"
        , "servant-client"
        , "servant-client-core"
        , "tasty"
        , "tasty-hedgehog"
        , "tmp-postgres"
        , "typed-process"
        , "validation-selective"
        , "transformers-base"
        , "uuid"
        , "containers"
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
      , tests.hokm-api-test = hokm-api-test
      }
