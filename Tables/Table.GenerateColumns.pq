let
    // Function
    TableGenerateColumnsFunction = (table as table, columns as list, newName as function, forEach as function) =>
        let
            newNameList = Value.ReplaceType(List.Transform(columns, newName), type {text}),

            AddRecordColumn = Table.AddColumn(table, "__New Columns__",
                (rec) =>
                Record.FromList(
                    List.Transform(columns, (c) => Function.Invoke(forEach, {
                        Record.Combine({rec,
                            [_ = [Value = Record.Field(rec, c),
                            Name = c]]})})),
                    newNameList
                    ),
                type record
                ),

            ExpandNewColumns = Table.ExpandRecordColumn(AddRecordColumn, "__New Columns__", newNameList, newNameList)

            // Some error handling
            , Check = [
                tryList = try newNameList,
                Result =
                    if tryList[HasError] then error [Reason= "Error in newName function", Message = tryList[Error][Message], Detail = tryList[Error][Detail]]
                    else if List.IsDistinct(tryList[Value]) 
                        then [Check = try ExpandNewColumns, 
                            Res =
                                if Check[HasError] then
                                    if Text.Contains(Check[Error][Message], "already exists in the record.")
                                        then error [Reason = "Expression.Error", Message = "Column '" & Check[Error][Detail][Name] & "' already exists in the table.", Detail= [Table = table]]
                                    else error Check[Error]
                                else Check[Value]][Res]
                    else error [Reason= "Error in newName function", Message = "Names generated are not distinct", Detail = [#"New names" = tryList[Value]]]
            ]

        in
            Check[Result],
            

    // Documentation
    TableGenerateColumnsType = 
        type function (
            table as (type table meta [
                Documentation.FieldCaption ="Target Table",
                Documentation.FieldDescription = "The target table"
                ]),
            columns as (type list meta [
                Documentation.FieldCaption = "Columns",
                Documentation.FieldDescription = "Columns used to generate new columns"
            ]),
            newName as ((type function( _ as any ) as any ) meta [
                Documentation.FieldCaption = "New name function",
                Documentation.FieldDescription = "Name generator"
            ]),
            forEach as ((type function( _ as any ) as any ) meta [
                Documentation.FieldCaption = "New column function",
                Documentation.FieldDescription = "Column generator"
            ])
        )
        as table meta
        [
            Documentation.Name = "Table.GenerateColumns",
            Documentation.Description = "Generates multiple columns",
            Documentation.LongDescription = "Generates multiple columns using a name generator and a column generator. Use <code>[_][Value]</code> for current column value and <code>[_][Name]</code> for current column name.<p><i>Note: If a column named <code>_</code> already exists in the table it won't work.</i>",
            Documentation.Category = "Table",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Generate columns named <code>""Start of Month *""</code>",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [id = 1, DateCreated = #date(2020,6,30), DateResolved = #date(2020, 6, 30)],
            [id = 1, DateCreated = #date(2020,5,25), DateResolved = #date(2020, 6, 12)],
            [id = 1, DateCreated = #date(2020,2,1), DateResolved = #date(2020, 7, 3)]
        })
in
    Table.GenerateColumns ( StartingTable, {""DateCreated"", ""DateResolved""}, (t) => ""Start of Month"" & t, each Date.StartOfMonth([_][Value]) )",
                Result = "Table.FromRecords({
    [id = 1, DateCreated = #date(2020,6,30), DateResolved = #date(2020, 6, 30), #""Start of Month DateCreated"" = #date(2020,6,1), #""Start of Month DateResolved"" = #date(2020, 6, 1)],
    [id = 1, DateCreated = #date(2020,5,25), DateResolved = #date(2020, 6, 12), #""Start of Month DateCreated"" = #date(2020,5,1), #""Start of Month DateResolved"" = #date(2020, 6, 1)],
    [id = 1, DateCreated = #date(2020,2,1), DateResolved = #date(2020, 7, 3), #""Start of Month DateCreated"" = #date(2020,2,1), #""Start of Month DateResolved"" = #date(2020, 7, 1)]
})"
            ]}
        ]

in
    Value.ReplaceType(TableGenerateColumnsFunction, TableGenerateColumnsType)