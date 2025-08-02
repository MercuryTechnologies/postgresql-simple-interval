module Database.PostgreSQL.Simple.Interval
  ( Unstable.Interval (..),

    -- * Constructors
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

    -- ** Saturating
    Unstable.fromMillisecondsSaturating,
    Unstable.fromSecondsSaturating,
    Unstable.fromMinutesSaturating,
    Unstable.fromHoursSaturating,
    Unstable.fromWeeksSaturating,
    Unstable.fromYearsSaturating,
  )
where

import qualified Database.PostgreSQL.Simple.Interval.Unstable as Unstable
