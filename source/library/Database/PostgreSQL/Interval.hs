module Database.PostgreSQL.Interval
  ( Unstable.Interval (..),
    Unstable.zero,
    Unstable.fromMicroseconds,
    Unstable.fromMilliseconds,
    Unstable.fromSeconds,
    Unstable.fromMinutes,
    Unstable.fromHours,
    Unstable.fromDays,
    Unstable.fromWeeks,
    Unstable.fromMonths,
    Unstable.fromYears,
  )
where

import qualified Database.PostgreSQL.Interval.Unstable as Unstable
