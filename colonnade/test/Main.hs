import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Colonnade/Encoding.hs"
  ]
