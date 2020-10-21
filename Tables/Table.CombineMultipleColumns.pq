let
    // Function
    TableCombineMultipleColumnsFunction = (table as table, optional skipStart as nullable number, optional skipEnd as nullable number, optional setSize as number, optional combiner as function, optional delimiter as nullable text, optional columnNameDelimiter as nullable text) as table =>
        let
            // Define things
            Names = List.RemoveLastN(Table.ColumnNames(table), skipEnd??0),
            groupSize = setSize??2,
            sep = delimiter??", ",
            afterN = skipStart??0,
            columnDelimiter = columnNameDelimiter??" & ",

            
            // Define Helper Functions
            TextCombiner = combiner??((x) => 
                let
                    t = Text.AfterDelimiter( List.Accumulate(x, "", (s,c) => s & sep & (Text.From(c)??"" ) ), sep )
                in
                    if t = sep then null else t),

            GetK = (k) => List.Accumulate(List.Range(Names, k, groupSize), {}, (s, c) =>  s & {c}),

            ColumnNameGenarator = (l as list) as text => Text.AfterDelimiter(List.Accumulate(l, "", (s, c) => s & columnDelimiter & c), columnDelimiter),

            // Error Handling
            CheckValues = [
                Check = (skipEnd??0)<0 or (setSize??2) <2 or (skipStart??0)<0, 
                Error = error 
                    [Reason = "Incorrect values",
                    Message = "Please supply positive values for skipStart and skipEnd and a setSize >= 2",
                    Detail = [#"Skip Start" = skipStart, #"Skip End" = skipEnd, #"Set Size" = setSize]]
            ],
            

            // Generate a list of the columns to be combined
            CombineList = List.Generate( () => [N = GetK(afterN), k = groupSize+afterN], each [k]-groupSize<List.Count(Names), each [N = GetK([k]), k = [k] + groupSize], each [N]),

            // Recursively combine columns. If in the end they're less, they will still be combined
            CombineColumns = (table, CombineList, k) => if List.IsEmpty(CombineList) then table else @CombineColumns(Table.CombineColumns(table, CombineList{0}, TextCombiner, ColumnNameGenarator(CombineList{0})), List.RemoveFirstN(CombineList, 1), k+1),
            
            Result = if CheckValues[Check] then CheckValues[Error] else CombineColumns(table, CombineList, 0)
        in
            Result,
        

    // Documentation
    TableCombineMultipleColumnsType = 
        type function (
            table as (type table meta [
                Documentation.FieldCaption ="Target table",
                Documentation.FieldDescription = "The target table"
                ]),
            optional skipStart as (type nullable number meta [
                Documentation.FieldCaption ="Skip first N",
                Documentation.FieldDescription = "Number of columns to skip from start"
                ]),
            optional skipEnd as (type nullable number meta [
                Documentation.FieldCaption ="Skip last N",
                Documentation.FieldDescription = "Number of columns to skip from end"
                ]),
            optional setSize as (type nullable number meta [
                Documentation.FieldCaption ="Group size",
                Documentation.FieldDescription = "Number of columns combine together"
                ]),
            optional combiner as (type function meta [
                Documentation.FieldCaption ="Combiner Function",
                Documentation.FieldDescription = "Optional Combiner"
                ]),
            optional delimiter as (type nullable text meta [
                Documentation.FieldCaption ="Delimiter",
                Documentation.FieldDescription = "Delimiter to use when combining columns"
                ]),
            optional columnNameDelimiter as (type nullable text meta [
                Documentation.FieldCaption ="Column Delimiter",
                Documentation.FieldDescription = "Delimiter to use when combining column names"
                ])
        )
        as table meta
        [
            Documentation.Name = "Table.CombineMultipleColumns",
            Documentation.Description = "Combines multiple columns at once using group size",
            Documentation.LongDescription = "Combines multiple columns at once using an optional <code>Group size</code>. Default group size is 2. If there are trailing columns less than <code>Group size</code> they will be combined in a smaller group.",
            Documentation.Category = "Table",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Combine all columns by 2",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [a = 1, b = 2],
            [a = 2, b = 2],
            [a = 3, b = 4],
            [a = 4, b = 5]
        })
in
    Table.CombineMultipleColumns( StartingTable )",
            Result = "Table.FromRecords({
    [#""a & b"" = 1, 2],
    [#""a & b"" = 2, 2],
    [#""a & b"" = 3, 4],
    [#""a & b"" = 4, 5]
})"
            ],[
                Description = "Combine all columns by 2, adding them",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [a = 1, b = 2],
            [a = 2, b = 2],
            [a = 3, b = 4],
            [a = 4, b = 5]
        })
in
    Table.CombineMultipleColumns( StartingTable, null, null, null, List.Sum )",
            Result = "Table.FromRecords({
    [#""a & b"" = 3],
    [#""a & b"" = 4],
    [#""a & b"" = 7],
    [#""a & b"" = 9]
})"
            ],[
                Description = "Combine all columns by 2, skipping first 2",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [a = 1, b = 2, c = ""apple"", d=  ""banana""],
            [a = 1, b = 2, c = ""cherry"", d=  ""chocolate""],
            [a = 1, b = 2, c = ""chocolate"", d=  ""banana""],
            [a = 1, b = 2, c = """", d=  ""banana""]
        })
in
    Table.CombineMultipleColumns( StartingTable, 2 )",
            Result = "Table.FromRecords({
    [a = 1, b = 2, #""c & d"" = ""apple, banana""],
    [a = 1, b = 2, #""c & d"" = ""cherry, chocolate""],
    [a = 1, b = 2, #""c & d"" = ""chocolate, banana""],
    [a = 1, b = 2, #""c & d"" = "", banana""]
})"
            ]}
        ]

in
    Value.ReplaceType(TableCombineMultipleColumnsFunction, TableCombineMultipleColumnsType)