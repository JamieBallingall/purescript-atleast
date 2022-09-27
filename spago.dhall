{ name = "atleast"
, dependencies =
  [ "arrays"
  , "effect"
  , "enums"
  , "fast-vect"
  , "foldable-traversable"
  , "maybe"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
