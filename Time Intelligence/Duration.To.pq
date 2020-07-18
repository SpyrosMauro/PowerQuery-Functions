let
    // Function
    DurationToFunction = (duration as nullable duration, durationTo as text) as nullable number =>
        let
            // durationTotal = Fix a Duration.TotalX function dependent on durationTo
            durationTotal = 
                let 
                    d = Text.Lower(durationTo)
                in
                    if d = "seconds" then Duration.TotalSeconds
                    else if d = "minutes" then Duration.TotalMinutes
                    else if d = "hours" then Duration.TotalHours
                    else if d = "days" then Duration.TotalDays
                    else if d = "weeks" then (m) => Value.Divide(Duration.TotalDays(m), 7)
                    else if d = "months" then (m) => Value.Divide(Duration.TotalDays(m), 30)
                    else if d = "quarters" then (m) => Value.Divide(Duration.TotalDays(m), 121)
                    else (m) => Value.Divide(Duration.TotalDays(m), 365),

            Result = durationTotal(duration)
        in
            Result,


    // Documentation
    DurationToType =
        type function (
            duration as (type nullable duration meta [
                Documentation.FieldCaption = "duration",
                Documentation.FieldDescription = "duration to transform"
            ]),
            durationTo as (type text meta[
                Documentation.FieldCaption = "Duration in",
                Documentation.FieldDescription = "Transform duration to",
                Documentation.AllowedValues = {"Seconds", "Minutes", "Hours", "Days", "Weeks", "Months", "Quarters", "Years"}
            ])
        )
        as nullable number meta
        [
            Documentation.Name = "Duration.To",
            Documentation.Description = "All duration functions in one",
            Documentation.LongDescription = "Transforms a given <code>duration</code> to a given interval (<code>durationTo</code>).<p>Default values:<table>
    <tr>
        <td><i>Transformation</i></td>
        <td><i>Days</i></td>
    </tr>
    <tr>
        <td>Weeks</td>
        <td><code>7</code></td>
    </tr>
    <tr>
        <td>Months<br></td>
        <td><code>30</code></td>
    </tr>
    <tr>
        <td>Quarters</td>
        <td><code>121</code></td>
    </tr>
    <tr>
        <td>Years</td>
        <td><code>365</code></td>
    </tr>
</table>",
            Documentation.Category = "Duration",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Duration to months",
                Code = "Duration.To(#duration(15,0,0,0), ""Months"")",
                Result = "0.5"
                ]}
        ]

in
    Value.ReplaceType(DurationToFunction, DurationToType)