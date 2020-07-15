let
    // Function
    TableReplaceValuesFromTableFunction = (table as table, transformationTable as table, optional columns as any, optional specifyTransformationColumns as list, optional missingField as nullable number) as table =>
        let
            // Buffer Transformation Table to call it only once
            BufferedTransformationTable = Table.Buffer(transformationTable),

            // Step to add error handling for given table & columns
            tryTransformationColumnNames = try
                let
                    temp = Table.ColumnNames(BufferedTransformationTable),
                    countTargetColumns = List.Count(temp),
                    countSpecifiedColumns = try List.Count(specifyTransformationColumns) otherwise 0
                in
                    if countTargetColumns < 2 then error [Reason = "", Message = "The transformation table needs to have at least two columns", Detail = [TransformationColumns = temp] ]
                    else if countSpecifiedColumns = 0 then {temp{0}, temp{1}}
                    else if countSpecifiedColumns = 2 then
                        let
                            l = List.Intersect({specifyTransformationColumns, temp})
                        in
                            if List.Count(l) = 2 then l else error  [Reason = "", Message = "The transformation table does not contain the 2 specified columns", Detail = [TransformationColumns = temp, SpecifiedColumns = specifyTransformationColumns] ]
                    else error [Reason = "Error in specified transformation columns", Message = "Please specify exactly 2 columns", Detail = [SpecifiedColumns = specifyTransformationColumns ]],
            TransformationColumnNames = tryTransformationColumnNames[Value],

            // Define Replacer Function to trick Table.ReplaceValue
            CustomReplacer =
                (value as any, old as any, new as any) as any =>
                    let
                        getvalue = try
                                Expression.Evaluate(
                                    "tbl{["
                                    & TransformationColumnNames{0}
                                    & " = t]}["
                                    & TransformationColumnNames{1}
                                    & "]",
                                    [ tbl = BufferedTransformationTable, t = value ]
                                ),
                        newvalue = 
                            if getvalue[HasError] then 
                                if Text.Contains(getvalue[Error][Message], "The key matched more than one row in the table") then error [Reason = "An error occured in the Transformation Table", Message = "More than one replacements were found for the specified value", Detail = [ValueToReplace = value]]
                                else if (missingField??1) = 0 then
                                    if Text.Contains(getvalue[Error][Message], "The key didn't match any rows in the table") then error [Reason = "An error occured in the Transformation Table", Message = "A replacement cannot be found for the specified value", Detail = [ValueToReplace = value]]
                                    else error getvalue[Error]
                                else if (missingField??1) = 2 then null
                                else value
                            else getvalue[Value]
                    in
                        newvalue,

            // If table and columns ok, call Table.ReplaceValue
            Result = if tryTransformationColumnNames[HasError] then error tryTransformationColumnNames[Error] else Table.ReplaceValue( table, null, null, CustomReplacer, columns??Table.ColumnNames(table) )
        in
            Result,


    // Documentation
    TableReplaceValuesFromTableType = 
        type function (
            table as (type table meta [
                Documentation.FieldCaption ="Target Table",
                Documentation.FieldDescription = "The table to make replacements"
                ]),
            transformationTable as (type table meta [
                Documentation.FieldCaption ="Transformation Table",
                Documentation.FieldDescription = "The lookup table for replacements"
                ]),
            optional columns as (type any meta [
                Documentation.FieldCaption ="Columns",
                Documentation.FieldDescription = "The columns in which to replace values"
                ]),
            optional specifyTransformationColumns as (type {text} meta [
                Documentation.FieldCaption ="Transformation Columns",
                Documentation.FieldDescription = "Columns to check on Transformation Table. If not specified, Column1 and Column2 are used."
                ]),
            optional missingField as MissingField.Type
            )
        as table meta
            [Documentation.Name = "Table.ReplaceValuesFromTable",
            Documentation.Description = "Replaces multiple values at once using a lookup table",
            Documentation.LongDescription = "Replaces multiple values at once using a lookup table.#(lf)If no columns are specified for the Transformation Table, the first column is used a a lookup and the second as a replacement. If no columns are specified in the Target Table then the default is all columns",
            Documentation.Category = "Table",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                    Description = "Replace values on all columns",
                    Code = "let
    StartingTable =
        Table.FromRecords({
            [a = 1, b = 2],
            [a = 2, b = 2],
            [a = 3, b = 4],
            [a = 4, b = 5]
        }),

    transformationTable = 
        Table.FromRecords({
            [check = 1, replace = ""A""],
            [check = 2, replace = ""B""],
            [check = 3, replace = ""C""],
            [check = 4, replace = ""D""]
        })
in
    Table.ReplaceValuesFromTable(StartingTable, transformationTable)",
                Result = "Table.FromRecords({
            [a = ""A"", b = ""B""],
            [a = ""B"", b = ""B""],
            [a = ""C"", b = ""D""],
            [a = ""D"", b = 5]
        })"
                ],
                [
                    Description = "Replace values on column 'a'",
                    Code = "let
    StartingTable =
        Table.FromRecords({
            [a = 1, b = 2],
            [a = 2, b = 2],
            [a = 3, b = 4],
            [a = 4, b = 5]
        }),

    transformationTable = 
        Table.FromRecords({
            [check = 1, replace = ""A""],
            [check = 2, replace = ""B""],
            [check = 3, replace = ""C""],
            [check = 4, replace = ""D""]
        })
in
    Table.ReplaceValuesFromTable(StartingTable, transformationTable, {""a""})",
                Result = "Table.FromRecords({
            [a = ""A"", b = 2],
            [a = ""B"", b = 2],
            [a = ""C"", b = 3],
            [a = ""D"", b = 5]
        })"
                ],
                [
                    Description = "Replace values on column 'b' by using 'Column2' column as lookup and 'Column1' as replacement",
                    Code = "let
    StartingTable =
        Table.FromRecords({
            [a = ""A"", b = ""B""],
            [a = ""B"", b = ""B""],
            [a = ""C"", b = ""D""],
            [a = ""D"", b = 5]
        }),

    transformationTable = 
        Table.FromRecords({
            [Column1 = 1, Column2 = ""A""],
            [Column1 = 2, Column2 = ""B""],
            [Column1 = 3, Column2 = ""C""],
            [Column1 = 4, Column2 = ""D""]
        })
in
    Table.ReplaceValuesFromTable(StartingTable, transformationTable, {""b""}, {""replace"", ""check""}, MissingField.Error)",
                Result = "Table.FromRecords({
            [a = ""A"", b = 2],
            [a = ""B"", b = 2],
            [a = ""C"", b = 4],
            [a = ""D"", b = error [Reason = """", Message = ""A replacement cannot be found for the specified value"", Detail = [ValueToReplace = 5]]
        })"
                ]}
        ]

in
    Value.ReplaceType(TableReplaceValuesFromTableFunction, TableReplaceValuesFromTableType)