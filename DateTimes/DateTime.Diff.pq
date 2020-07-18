let
    // Function
    DateTimeDiffFunction = (dateStart as nullable datetime, dateEnd as nullable datetime, optional excludedDays as list, optional timeStart as time, optional timeEnd as time, optional durationTo as text) =>
        let
            // Helper Functions
            // durationTotal = Fix a Duration.TotalX function dependent on durationTo
            durationTotal = 
                let 
                    d = Text.Lower(durationTo)
                in
                    if d = null then (m) => m
                    else if d = "seconds" then Duration.TotalSeconds
                    else if d = "minutes" then Duration.TotalMinutes
                    else if d = "hours" then Duration.TotalHours
                    else if d = "days" then Duration.TotalDays
                    else if d = "weeks" then (m) => Value.Divide(Duration.TotalDays(m), 7)
                    else if d = "months" then (m) => Value.Divide(Duration.TotalDays(m), 30)
                    else if d = "quarters" then (m) => Value.Divide(Duration.TotalDays(m), 121)
                    else (m) => Value.Divide(Duration.TotalDays(m), 365),

            // dayIn: find relevant times in day
            dayIn = (timeStart, timeEnd) as list =>
                let
                    times = { timeStart??#time(0,0,0) , timeEnd??#time(24,0,0) }
                in
                    if times{0} >= times{1}
                        then error [Reason= "Incorrect time limits", Message = "'Start time' must be smaller than 'End time'", Detail = [#"Start time" = timeStart, #"End time" = timeEnd]]
                    else times,

            // dayDuration
            dayDuration = (times) as duration => ( #date(2020,1,1) & times{1} ) - ( #date(2020,1,1) & times{0} ),

            // findExcludedDuration: Find duration to exclude based on excludedDays
            findExcludedDuration = (dates as list, startDateTime as datetime, endDateTime as datetime, optional times as list) as duration =>
                let
                    wholeDate = dayDuration(times),
                    limitDurations = [
                        check = startDate = endDate,
                        start = (startDate & times{1} - startDateTime),
                        end =  (endDateTime - (endDate & times{0})) ],
                    result =
                        if limitDurations[check] then List.Count(List.Select(dates, each _ = startDate)) * (endDateTime - startDateTime)
                        else List.Accumulate(dates, #duration(0,0,0,0),
                            (sum, current) =>
                                if current < startDate or current > endDate then sum
                                else if current = startDate then sum + limitDurations[start]
                                else if current = endDate then sum + limitDurations[end]
                                else sum + wholeDate )
                in
                    result,
                

            // Start finding things
            // Sort datetimes and keep direction
            dateSort = List.Sort({dateStart, dateEnd}),
            direction = if (dateStart = dateSort{0}) then 1 else -1,

            // Find relevant times in day
            times = dayIn(timeStart, timeEnd),

            // Calculate date components to use
            startDate = DateTime.Date(dateSort{0}),
            endDate = DateTime.Date(dateSort{1}),

            // Truncate datetimes
            startDateTime = startDate & List.Median(times & {DateTime.Time(dateSort{0})}),
            endDateTime = endDate & List.Median(times & {DateTime.Time(dateSort{1})}),
            
            // Find duration
            calcDuration =
                let
                    ExcludedDuration = findExcludedDuration(excludedDays??{}, startDateTime, endDateTime, times),
                    AllDuration = endDateTime - startDateTime,
                    NonWorkingDuration = Duration.Days(endDate - startDate) * (#duration(1,0,0,0) - dayDuration(times))
                in
                    AllDuration - NonWorkingDuration - ExcludedDuration,
            fixDuration = durationTotal(calcDuration) * direction,

            Result = if dateStart = null or dateEnd = null then null else fixDuration
        in
            Result,


    // Documentation
    DateTimeDiffType =
        type function (
            dateStart as (type nullable datetime meta[
                Documentation.FieldCaption = "Start date",
                Documentation.FieldDescription = "DateTime Start"
            ]),
            dateEnd as (type nullable datetime meta[
                Documentation.FieldCaption = "End date",
                Documentation.FieldDescription = "DateTime End"
            ]),
            optional excludedDays as (type {date} meta[
                Documentation.FieldCaption = "Excluded Dates",
                Documentation.FieldDescription = "A list of dates to exclude"
            ]),
            optional timeStart as (type time meta[
                Documentation.FieldCaption = "Start time",
                Documentation.FieldDescription = "When a day starts counting"
            ]),
            optional timeEnd as (type time meta[
                Documentation.FieldCaption = "End time",
                Documentation.FieldDescription = "When a day stops counting"
            ]),
            optional durationTo as (type text meta[
                Documentation.FieldCaption = "Duration in",
                Documentation.FieldDescription = "Transform duration to",
                Documentation.AllowedValues = {"Seconds", "Minutes", "Hours", "Days", "Weeks", "Months", "Quarters", "Years"}
            ])
        )
        as nullable any meta
        [
            Documentation.Name = "DateTime.Diff",
            Documentation.Description = "Finds the difference between two datetimes",
            Documentation.LongDescription = "Finds the difference between two datetimes. Optional 'Excluded Dates' list. Optional 'Start time' and 'End time'.",
            Documentation.Category = "Duration",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Diff using excluded date list",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [start = #datetime(2020,7,1,0,0,0), end = #datetime(2020,7,2,15,0,0)],
            [start = #datetime(2020,7,1,5,0,0), end = #datetime(2020,7,2,19,0,0)],
            [start = #datetime(2020,7,1,8,0,0), end = #datetime(2020,7,2,16,0,0)],
            [start = #datetime(2020,7,1,16,0,0), end = #datetime(2020,7,3,7,0,0)]
        }),
    holidays = {#date(2020,7,2)}
in
    Table.AddColumns( StartingTable, ""diff"", each DateTime.Diff( [end], [start], holidays ) )",
                Result = "Table.FromRecords({
    [start = #datetime(2020,7,1,0,0,0), end = #datetime(2020,7,2,15,0,0), diff = #duration(1,0,0,0)],
    [start = #datetime(2020,7,1,5,0,0), end = #datetime(2020,7,2,19,0,0), diff = #duration(19,0,0,0)],
    [start = #datetime(2020,7,1,8,0,0), end = #datetime(2020,7,3,16,0,0), diff = #duration(1,8,0,0)],
    [start = #datetime(2020,7,1,16,0,0), end = #datetime(2020,7,3,7,0,0), diff = #duration(0,15,0,0)]
})"
            ],[
                Description = "Diff using excluded date list and time limits",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [start = #datetime(2020,7,1,0,0,0), end = #datetime(2020,7,2,15,0,0)],
            [start = #datetime(2020,7,1,5,0,0), end = #datetime(2020,7,2,19,0,0)],
            [start = #datetime(2020,7,1,8,0,0), end = #datetime(2020,7,2,16,0,0)],
            [start = #datetime(2020,7,1,16,0,0), end = #datetime(2020,7,3,7,0,0)]
        }),
    holidays = {#date(2020,7,2)}
in
    Table.AddColumns( StartingTable, ""diff"", each DateTime.Diff( [end], [start], holidays, #time(8,0,0), #time(16,0,0) ) )",
                Result = "Table.FromRecords({
    [start = #datetime(2020,7,1,0,0,0), end = #datetime(2020,7,2,15,0,0), diff = #duration(0,8,0,0)],
    [start = #datetime(2020,7,1,5,0,0), end = #datetime(2020,7,2,19,0,0), diff = #duration(0,8,0,0)],
    [start = #datetime(2020,7,1,8,0,0), end = #datetime(2020,7,3,16,0,0), diff = #duration(0,16,0,0)],
    [start = #datetime(2020,7,1,16,0,0), end = #datetime(2020,7,3,7,0,0), diff = #duration(0,0,0,0)]
})"
            ],[
                Description = "Diff using time limits",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [start = #datetime(2020,7,1,0,0,0), end = #datetime(2020,7,2,15,0,0)],
            [start = #datetime(2020,7,1,5,0,0), end = #datetime(2020,7,2,19,0,0)],
            [start = #datetime(2020,7,1,8,0,0), end = #datetime(2020,7,2,16,0,0)],
            [start = #datetime(2020,7,1,16,0,0), end = #datetime(2020,7,3,7,0,0)]
        })
in
    Table.AddColumns( StartingTable, ""diff"", each DateTime.Diff( [end], [start], null, #time(8,0,0), #time(16,0,0) ) )",
                Result = "Table.FromRecords({
    [start = #datetime(2020,7,1,0,0,0), end = #datetime(2020,7,2,15,0,0), diff = #duration(0,15,0,0)],
    [start = #datetime(2020,7,1,5,0,0), end = #datetime(2020,7,2,19,0,0), diff = #duration(0,16,0,0)],
    [start = #datetime(2020,7,1,8,0,0), end = #datetime(2020,7,3,16,0,0), diff = #duration(1,0,0,0)],
    [start = #datetime(2020,7,1,16,0,0), end = #datetime(2020,7,3,7,0,0), diff = #duration(0,8,0,0)]
})"
            ]}
        ]

in
    Value.ReplaceType(DateTimeDiffFunction, DateTimeDiffType)