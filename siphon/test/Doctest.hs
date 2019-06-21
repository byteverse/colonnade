import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Siphon.hs"
  ]

