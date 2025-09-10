// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.datetime
import test.runner
import test.unit as unit

inline time_day_t to_time_day_t(time_t t)
{
    time_day_t d;
    d.day = t.day;
    d.month = t.month;
    d.year = t.year;
    return d;
}

inline bool BasicTest()
{
    bool returnValue = true;

    // Minimum input should return 0
    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = 0;
        t.hours = 0;
        t.day = 0;
        t.month = 0;
        t.year = 1970;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_days_to_epoch(to_time_day_t(t));

        const auto expected = 0;
        const auto expectedDays = 0;

        returnValue &&= result.is_valid;
        returnValue &&= (result.value == expected);
        returnValue &&= resultDays.is_valid;
        returnValue &&= (resultDays.value == expectedDays);
    }

    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = 0;
        t.hours = 0;
        t.day = 0;
        t.month = 0;
        t.year = 1980;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_days_to_epoch(to_time_day_t(t));

        const auto expected = 315532800;
        const auto expectedDays = 3652;

        returnValue &&= result.is_valid;
        returnValue &&= (result.value == expected);
        returnValue &&= resultDays.is_valid;
        returnValue &&= (resultDays.value == expectedDays);
    }

    {
        time_t t = {};

        t.seconds = 23;
        t.minutes = 1;
        t.hours = 3;
        t.day = 10;
        t.month = 1;
        t.year = 2020;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_days_to_epoch(to_time_day_t(t));

        const auto expected = 1581390083;
        const auto expectedDays = 18303;

        returnValue &&= result.is_valid;
        returnValue &&= (result.value == expected);
        returnValue &&= resultDays.is_valid;
        returnValue &&= (resultDays.value == expectedDays);
    }

    // Maximum allowed value
    {
        time_t t = {};

        t.seconds = 59;
        t.minutes = 59;
        t.hours = 23;
        t.day = 30;
        t.month = 11;
        t.year = 2993;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_days_to_epoch(to_time_day_t(t));

        const auto expected = 32314377599;
        const auto expectedDays = 374008;

        returnValue &&= result.is_valid;
        returnValue &&= (result.value == expected);
        returnValue &&= resultDays.is_valid;
        returnValue &&= (resultDays.value == expectedDays);
    }

    return returnValue;
}

inline bool YearRangeTest()
{
    bool returnValue = true;

    // Years before 1970 fail
    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = 0;
        t.hours = 0;
        t.day = 0;
        t.month = 0;
        t.year = 1969;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_days_to_epoch(to_time_day_t(t));

        returnValue &&= !result.is_valid;
        returnValue &&= (result.value == 0);
        returnValue &&= !resultDays.is_valid;
        returnValue &&= (resultDays.value == 0);
    }

    // Years after 2993 fail
    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = 0;
        t.hours = 0;
        t.day = 0;
        t.month = 0;
        t.year = 2994;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_days_to_epoch(to_time_day_t(t));

        returnValue &&= !result.is_valid;
        returnValue &&= (result.value == 0);
        returnValue &&= !resultDays.is_valid;
        returnValue &&= (resultDays.value == 0);
    }

    return returnValue;
}

inline bool MonthRangeTest()
{
    bool returnValue = true;

    for (const auto i : 16)
    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = 0;
        t.hours = 0;
        t.day = 0;
        t.month = i;
        t.year = 2000;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_days_to_epoch(to_time_day_t(t));

        if (i < 12)
        {
            // valid month
            returnValue &&= result.is_valid;
            returnValue &&= resultDays.is_valid;
        }
        else
        {
            // invalid month
            returnValue &&= !result.is_valid;
            returnValue &&= !resultDays.is_valid;
        }
    }

    return returnValue;
}

inline bool HourRangeTest()
{
    bool returnValue = true;

    for (const auto i : 32)
    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = 0;
        t.hours = i;
        t.day = 0;
        t.month = 0;
        t.year = 2000;

        const auto result = utc_time_to_epoch(t);

        if (i < 24)
        {
            // valid hour
            returnValue &&= result.is_valid;
        }
        else
        {
            // invalid hour
            returnValue &&= !result.is_valid;
        }
    }

    return returnValue;
}

inline bool MinuteRangeTest()
{
    bool returnValue = true;

    for (const auto i : 64)
    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = i;
        t.hours = 0;
        t.day = 0;
        t.month = 0;
        t.year = 2000;

        const auto result = utc_time_to_epoch(t);

        if (i < 60)
        {
            // valid minute
            returnValue &&= result.is_valid;
        }
        else
        {
            // invalid minute
            returnValue &&= !result.is_valid;
        }
    }

    return returnValue;
}

inline bool SecondRangeTest()
{
    bool returnValue = true;

    for (const auto i : 64)
    {
        time_t t = {};

        t.seconds = i;
        t.minutes = 0;
        t.hours = 0;
        t.day = 0;
        t.month = 0;
        t.year = 2000;

        const auto result = utc_time_to_epoch(t);

        if (i < 60)
        {
            // valid second
            returnValue &&= result.is_valid;
        }
        else
        {
            // invalid second
            returnValue &&= !result.is_valid;
        }
    }

    return returnValue;
}

inline bool DayRangeTestHelper(
    uint4 month,
    uint12 year,
    uint5 expectedDaysPerMonth)
{
    bool returnValue = true;

    for (const auto i : 32)
    {
        time_t t = {};

        t.seconds = 0;
        t.minutes = 0;
        t.hours = 0;
        t.day = i;
        t.month = month;
        t.year = year;

        const auto result = utc_time_to_epoch(t);
        const auto resultDays = utc_time_to_epoch(t);

        if (i < expectedDaysPerMonth)
        {
            // valid day
            returnValue &&= result.is_valid;
            returnValue &&= resultDays.is_valid;
        }
        else
        {
            // invalid day
            returnValue &&= !result.is_valid;
            returnValue &&= !resultDays.is_valid;
        }
    }

    return returnValue;
}

inline bool DayRangeTest()
{
    bool returnValue = true;

    // Jan
    returnValue &&= DayRangeTestHelper(0, 1999, 31);
    returnValue &&= DayRangeTestHelper(0, 2000, 31);

    // Feb - not a leap year
    returnValue &&= DayRangeTestHelper(1, 1999, 28);

    // Feb - leap year
    returnValue &&= DayRangeTestHelper(1, 2000, 29);

    // Feb - not a leap year (because the year is divisible by 100 but not by 400)
    returnValue &&= DayRangeTestHelper(1, 2100, 28);

    // March
    returnValue &&= DayRangeTestHelper(2, 1999, 31);
    returnValue &&= DayRangeTestHelper(2, 2000, 31);

    // April
    returnValue &&= DayRangeTestHelper(3, 1999, 30);
    returnValue &&= DayRangeTestHelper(3, 2000, 30);

    // May
    returnValue &&= DayRangeTestHelper(4, 1999, 31);
    returnValue &&= DayRangeTestHelper(4, 2000, 31);

    // June
    returnValue &&= DayRangeTestHelper(5, 1999, 30);
    returnValue &&= DayRangeTestHelper(5, 2000, 30);

    // July
    returnValue &&= DayRangeTestHelper(6, 1999, 31);
    returnValue &&= DayRangeTestHelper(6, 2000, 31);

    // August
    returnValue &&= DayRangeTestHelper(7, 1999, 31);
    returnValue &&= DayRangeTestHelper(7, 2000, 31);

    // September
    returnValue &&= DayRangeTestHelper(8, 1999, 30);
    returnValue &&= DayRangeTestHelper(8, 2000, 30);

    // October
    returnValue &&= DayRangeTestHelper(9, 1999, 31);
    returnValue &&= DayRangeTestHelper(9, 2000, 31);

    // November
    returnValue &&= DayRangeTestHelper(10, 1999, 30);
    returnValue &&= DayRangeTestHelper(10, 2000, 30);

    // December
    returnValue &&= DayRangeTestHelper(11, 1999, 31);
    returnValue &&= DayRangeTestHelper(11, 2000, 31);

    return returnValue;
}

inline void test_main()
{
    unit::check<1>(BasicTest);
    unit::check<2>(YearRangeTest);
    unit::check<3>(MonthRangeTest);
    unit::check<4>(HourRangeTest);
    unit::check<5>(MinuteRangeTest);
    unit::check<6>(SecondRangeTest);
    unit::check<7>(DayRangeTest);
}
