let Hokm = ../package.dhall

let dependencies =
      [{ name = "base", mixin = "hiding (Prelude)" }
      ]

let hokm-prelude =
      { source-dirs = "src"
      , dependencies =
        [ "aeson"
        , "ansi-terminal"
        , "bytestring"
        , "co-log"
        , "co-log-core"
        , "containers"
        , "chronos"
        , "flow"
        , "lens"
        , "relude"
        , "text"
        , "uuid"
        , "validation-selective"
        , "vector"
        , "word8"
        ]
      }

in    Hokm.package
    ⫽ { name = "hokm-prelude"
      , synopsis = "Hokm custom prelude"
      , description = "Hokm custom prelude"
      , dependencies
      , library = hokm-prelude
      }
