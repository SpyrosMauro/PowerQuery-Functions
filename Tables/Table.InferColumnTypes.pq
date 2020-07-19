let
    // Function
    TableInferColumnTypesFunction = (table as table, optional columns as list, optional precision as number, optional noNull as logical, optional applyAll as logical) as table =>
        let
            // Helper Functions
            // typeFinder: finds the main type of a list
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

            // fakeFunction: takes a type and makes a function that returns this type, to be used in case of applyAll
            // when Table.TransformColumnTypes is able to ascribe these 6 types then it will be redundant
            fakeFunction = (Type as type) as function =>
                if Type.Is(Type, type null) then (x) as null => null
                else if Type.Is(Type, type function) then (x) as function => x
                else if Type.Is(Type, type record) then (x) as record => x
                else if Type.Is(Type, type table) then (x) as table => x
                else if Type.Is(Type, type list) then (x) as list => x
                else if Type.Is(Type, type type) then (x) as type => x
                else (x) => x,


            // Get types
            columnNames = columns??Table.ColumnNames(table),
            rowsToGet = List.Min({Table.RowCount(table), precision??500, 1000}),

            tableValuesToType = Table.ReplaceValue(Table.FirstN((if columns = null then table else Table.SelectColumns(table, columnNames)), rowsToGet), null, null, (x,y,z) => Value.Type(x), columnNames),
            FindBestType = Table.CombineColumns(
                Table.Transpose(tableValuesToType),
                List.Transform({1..rowsToGet}, each "Column"&Text.From(_)),
                typeFinder,
                "Types"
                ),
            
            // Apply transformations
            allTransformations = List.Zip({columnNames, FindBestType[Types]}),
            applicableTransformations = List.Select(allTransformations,
                each not ( Type.Is(_{1}, type null) or Type.Is(_{1}, type function) or Type.Is(_{1}, type record) or Type.Is(_{1}, type table) or Type.Is(_{1}, type list) or Type.Is(_{1}, type type) )),
            applySimpleTransformations = Table.TransformColumnTypes(table, applicableTransformations),

            applyAllList = List.Transform(List.RemoveItems(allTransformations, applicableTransformations), each {_{0}, fakeFunction(_{1}), _{1}}),
            applyOthers = Table.TransformColumns(applySimpleTransformations, applyAllList),

            Result = if applyAll??true then applyOthers else applySimpleTransformations
        in
            Result,


    // Documentation
    TableInferColumnTypesType =
        type function (
            table as (type table meta [
                Documentation.FieldCaption = "Target table",
                Documentation.FieldDescription = "The table to make type replacements"
                ]),
            optional columns as (type list meta [
                Documentation.FieldCaption = "Columns",
                Documentation.FieldDescription = "The columns to make type replacements"
                ]),
            optional precision as (type number meta [
                Documentation.FieldCaption = "precision",
                Documentation.FieldDescription = "First <code>n</code> rows to check for best type match. Default: <code>500<c/ode>."
                ]),
            optional noNull as (type logical meta [
                Documentation.FieldCaption = "noNull",
                Documentation.FieldDescription = "Switch to type null usage. If true (default) then <code>type null = type any</code>."
            ]),
            optional applyAll as (type logical meta [
                Documentation.FieldCaption = "applyAll",
                Documentation.FieldDescription = "Switch to apply extra replacements for special types (e.g. type type). Default: <code>true</code>."
            ])
        )
        as table meta
        [
            Documentation.Name = "Table.InferColumnTypes",
            Documentation.Description = "Infers column types",
            Documentation.LongDescription = "Infers column types. Optional <code>precision</code>.<p><i>Only primitive types supported.</i>",
            Documentation.Category = "Table",
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
    Table.InferColumnTypes ( StartTable )",
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
    Table.InferColumnTypes ( StartTable )",
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
    Value.ReplaceType(TableInferColumnTypesFunction, TableInferColumnTypesType)