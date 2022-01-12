module HW1.T1
  ( Day (..),
    nextDay,
    afterDays,
    isWeekend,
    daysToParty,
  )
where

import Numeric.Natural (Natural)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

dec :: Natural -> Natural
dec = subtract 1

inc :: Natural -> Natural
inc = (1 +)

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays cnt day = afterDays (dec cnt) (nextDay day)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend day = case day of
  Saturday -> True
  Sunday -> True
  _ -> False

-- | Computes the number of days until the next Friday.
daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day = inc $ daysToParty $ nextDay day
