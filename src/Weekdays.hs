module Weekdays where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

weekdaysThisMonth :: IO Int
weekdaysThisMonth =
  do
    utcTime <- getCurrentTime
    let today = utctDay utcTime
    let (year, month, _) = toGregorian today
    return 1

getAllWorkdaysOfMonth :: Integer -> Int -> [Day]
getAllWorkdaysOfMonth year month =
  let lengthOfMonth = gregorianMonthLength year month in
  let allDays = fromGregorian year month <$> [1..(lengthOfMonth + 1)] in
  filter (\day -> let (_,_,dayNumber) = toWeekDate day in dayNumber < 6) allDays

