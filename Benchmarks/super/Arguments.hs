module Arguments where
 
randomXS = \level -> case level of
  1 -> [1..10]
  2 -> [10..100]
  3 -> [100..1000]