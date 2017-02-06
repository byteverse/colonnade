import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Text/Blaze/Colonnade.hs"
  ]
