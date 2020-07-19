let
    // Function
    TypeTableTypeFunction = (table as table, optional precision as number) as any =>
        let
            columnNames = Table.ColumnNames(table),

            //typeFinder: finds the main type of a list
            typeFinder = (x as list, optional noNull as logical, optional crazyMode as logical) =>
                    let
                        startingList = if noNull??true then List.Select(x, each not Type.Is(_, type null)) else x,
                        listMode = List.Mode(startingList),
                        listDistinct = List.Distinct(startingList),
                        countDistinct = List.Count(listDistinct)
                    in
                    // Three extra steps here because type {number} != type {text}
                    if List.AnyTrue(List.Transform(listDistinct, each Type.Is(_, type [...]))) then type [...]
                    else if List.AnyTrue(List.Transform(listDistinct, each Type.Is(_, type table))) then type table
                    else if List.AnyTrue(List.Transform(listDistinct, each Type.Is(_, type list))) then type list
                    else if List.AnyTrue(List.Transform(listDistinct, each Type.Is(_, type function))) then type function
                    else if crazyMode??false then listMode
                    else if countDistinct > 3 then type any
                    else if countDistinct > 2 then 
                        let
                            newList = List.Select(listDistinct, each _ <> type any),
                            countNew = List.Count(newList)
                        in
                            if countNew = 2 and List.Contains(listDistinct, type text) then type text
                            else type any
                    else if countDistinct > 1 then
                        if List.Contains(listDistinct, type any)
                            then List.Select(listDistinct, each _ <> type any){0}
                        else if List.Contains(listDistinct, type text) then type text
                        else type any
                    else if countDistinct = 1 then listDistinct{0}
                    else type any,
                    
            findType =
                if (precision??0) > 0 then
                    let
                        rowsToGet = List.Min({Table.RowCount(table), precision??50, 1000}),

                        tableValuesToType = Table.ReplaceValue(Table.FirstN(table, rowsToGet), null, null, (x,y,z) => Value.Type(x), columnNames),
                        FindBestType = Table.CombineColumns(
                            Table.Transpose(tableValuesToType),
                            List.Transform({1..rowsToGet}, each "Column"&Text.From(_)),
                            typeFinder,
                            "Types"
                            ),
                        
                        Zip = List.Zip({columnNames, List.Transform(FindBestType[Types], each if Type.Is(_, type null) then type any else _)}),
                        realFakeTables = Table.TransformColumnTypes(#table(List.Transform(Zip, each _{0}), {}), Zip),
                        schema = Table.Schema(realFakeTables)[[Name], [Kind]]
                    in
                        schema
                else
                    Table.Schema(table)[[Name], [Kind]],

            tableType =
                "type table [#(lf)#(tab)"
                & Text.Combine(Table.ToList(findType, (x) => "#""" & x{0} &"""" & " = ( type " & x{1} & " )" ), ",#(lf)#(tab)")
                & "#(lf)]"
        in
            tableType,


    // Documentation
    TypeTableTypeType =
        type function (
            table as (type table meta [
                Documentation.FieldCaption = "Target table",
                Documentation.FieldDescription = "The table for which to generate <code>Table.View</code>"
                ]),
            optional precision as (type number meta [
                Documentation.FieldCaption = "precision",
                Documentation.FieldDescription = "First <code>n</code> rows to check for best type match"
                ])
        )
        as text meta
        [
            Documentation.Name = "Type.TableType",
            Documentation.Description = "Generates the text of non abstract table type",
            Documentation.LongDescription = "Generates <b>text of</b> <code>non abstract type table</code>. Optional <code>precision</code>.<p> <i>Only primitive types supported.</i>",
            Documentation.Category = "Type",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Table type to text",
                Code = "let
    StartTable = Table.FromRecords({
        [start = #datetime(2020,7,1,0,0,0), diff = #duration(0,15,0,0), list = {1}],
        [start = #datetime(2020,7,1,5,0,0), diff = #duration(0,16,0,0), list = null],
        [start = #datetime(2020,7,1,8,0,0), diff = #duration(1,0,0,0), list = {4}],
        [start = #datetime(2020,7,1,16,0,0), diff = #duration(0,8,0,0), list = {3,4}]
    })
in
    Type.TableType ( StartTable )",
                Result = "type table [
	#""start"" = ( type datetime ),
	#""diff"" = ( type duration ),
	#""list"" = ( type list )
]"
            ],[
                Description = "Table type to text",
                Code = "let
    startTable = #table(
        {""n"", ""fun"", ""rec"", ""tb"", ""l"", ""bin"", ""typ""},
        {{1, (x) => x, [a = 1], #table({""a""}, {{1}}), {1..3}, #binary(""), type text}}
        )
in
    Type.TableType ( StartTable )",
                Result = "type table [
	#""n"" = ( type number ),
	#""fun"" = ( type function ),
	#""rec"" = ( type record ),
	#""tb"" = ( type table ),
	#""l"" = ( type list ),
	#""bin"" = ( type binary ),
	#""typ"" = ( type type )
]"
            ]}
        ]

in
    Value.ReplaceType(TypeTableTypeFunction, TypeTableTypeType)