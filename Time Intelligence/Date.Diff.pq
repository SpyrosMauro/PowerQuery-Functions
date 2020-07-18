let
    // Function
    DateDiffFunction = (dateStart as nullable date, dateEnd as nullable date, optional excludedDays as list) as nullable duration =>
        let
            // Helper Functions
            // findExcludedDuration: Find duration to exclude based on excludedDays
            findExcludedDuration = (dates as list, startDate as date, endDate as date) as duration =>
                let
                    countDays = List.Count(List.Select(excludedDays??{}, each _>= startDate and _ <= endDate)),
                    result = countDays * #duration(1,0,0,0)
                in
                    result,
                

            // Start finding things
            // Sort dates and keep direction
            dateSort = List.Sort({dateStart, dateEnd}),
            direction = if (dateStart = dateSort{0}) then 1 else -1,

            // Calculate dates to use
            startDate = dateSort{0},
            endDate = dateSort{1},
            
            // Find duration
            calcDuration =
                let
                    ExcludedDuration = findExcludedDuration(excludedDays??{}, startDate, endDate),
                    AllDuration = endDate - startDate
                in
                    if AllDuration = #duration(0,0,0,0) then AllDuration else AllDuration - ExcludedDuration,
            fixDuration = calcDuration * direction,

            Result = if dateStart = null or dateEnd = null then null else fixDuration
        in
            Result,


    // Documentation
    DateDiffType =
        type function (
            dateStart as (type nullable date meta[
                Documentation.FieldCaption = "Start date",
                Documentation.FieldDescription = "Date Start"
            ]),
            dateEnd as (type nullable date meta[
                Documentation.FieldCaption = "End date",
                Documentation.FieldDescription = "Date End"
            ]),
            optional excludedDays as (type {date} meta[
                Documentation.FieldCaption = "Excluded Dates",
                Documentation.FieldDescription = "A list of dates to exclude"
            ])
        )
        as nullable duration meta
        [
            Documentation.Name = "Date.Diff",
            Documentation.Description = "Finds the difference between two dates",
            Documentation.LongDescription = "Finds the difference between two dates. Optional <code>'Excluded Dates'</code> list.",
            Documentation.Category = "Duration",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Diff using excluded date list",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [start = #date(2020,7,1), end = #date(2020,7,2)],
            [start = #date(2020,7,1), end = #date(2020,7,3)],
            [start = #date(2020,7,5), end = #date(2020,7,2)]
        }),
    holidays = {#date(2020,7,2), #date(2020,7,4)}
in
    Table.AddColumns( StartingTable, ""diff"", each DateTime.Diff( [end], [start], holidays ) )",
                Result = "Table.FromRecords({
    [start = #date(2020,7,1), end = #datetime(2020,7,2), diff = #duration(0,0,0,0)],
    [start = #date(2020,7,1), end = #datetime(2020,7,3), diff = #duration(1,0,0,0)],
    [start = #date(2020,7,5), end = #datetime(2020,7,2), diff = #duration(-1,0,0,0)]
})"
            ]}
        ]

in
    Value.ReplaceType(DateDiffFunction, DateDiffType)