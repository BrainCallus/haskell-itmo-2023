import HW1.T1
import HW1.T2
import HW1.T3

main :: IO ()
test :: Bool -> IO ()
test cond = putStrLn (if cond then "OK" else "FAIL")
main = do
  test $ isWeekend Saturday && isWeekend Sunday
  let week = [Monday, Tuesday, Wednesday, Thursday, Friday]
  test $ not $ any isWeekend week
  let m = maximum (map daysToParty (week ++ [Saturday, Sunday]))
  test $ m == 6
  test $ daysToParty Friday == 0

  putStrLn "\nPrimitive recursive functions some tests"
  putStrLn $ case nsub (nFromNatural 0) (nFromNatural 0) of
    Just _ -> "OK"
    _ -> "FAIL"

  putStrLn $ case nsub (nFromNatural 0) (nFromNatural 1) of
    Just _ -> "FAIL"
    _ -> "OK"
  test $ nToNum (ndiv (nFromNatural 3) (nFromNatural 2)) == 1
  print $ nToNum (ndiv (nFromNatural 3) (nFromNatural 2))
  test $ nToNum (ndiv (nFromNatural 6) (nFromNatural 7)) == 0
  test $ nToNum (ndiv (nFromNatural 10) (nFromNatural 5)) == 2
  test $ nToNum (ndiv (nFromNatural 10) (nFromNatural 3)) == 3
  test $ nToNum (ndiv (nFromNatural 17) (nFromNatural 7)) == 2
  test $ nToNum (ndiv (nFromNatural 100) (nFromNatural 9)) == 11

  putStrLn "\nTREE_TEST"
  let tree = tinsert 20 (tinsert 1 (tFromList [-9, -10, 4, 1, 0, 89, -66, 45, 46, 28, -10, 0, 66]))
  putStrLn (show tree)
  test $ tdepth tree == 5
  test $ tsize tree == 12
  test $ tmember 0 tree && tmember 4 tree && not (tmember (-1) tree)
