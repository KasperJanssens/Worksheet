module Weekdays where

import Data.Time.Clock
import Data.Time.Calendar

weekdaysThisMonth :: IO Int
weekdaysThisMonth =
  do
    utcTime <- getCurrentTime
    let today = utctDay utcTime
    let (year, month, _) = toGregorian today
    return 1

getAllDaysOfMonth :: Integer -> Int -> [Int]
getAllDaysOfMonth year month =
  let lengthOfMonth = gregorianMonthLength year month in
  let allDays = fromGregorian year month <$> [1..(lengthOfMonth + 1)] in
  undefined

