let
    // Function
    TableInferColumnTypesFunction = (table as table, optional columns as list, optional precision as number, optional applyOnlyNormalTypes as logical, optional tableName as text) as any =>
        let
            // Helper Functions
            // typeFinder: finds the main type of a list
            typeFinder = (x as list, optional crazyMode as logical) =>
                let
                    startingList = List.Select(x, each _ <> type any),
                    listMode = List.Mode(startingList),
                    listDistinct = List.Distinct(startingList),
                    countDistinct = List.Count(listDistinct),
                    removeInts = List.RemoveItems(listDistinct, {Int64.Type, Int32.Type, Int8.Type})
                in
                if countDistinct = 0 then type any
                else if countDistinct = 1 then listDistinct{0}
                // Five extra steps here for abstract types
                else if List.AllTrue(List.Transform(listDistinct, each Type.Is(_, type number))) then type number
                else if List.AllTrue(List.Transform(listDistinct, each Type.Is(_, type [...]))) then type nullable [...]
                else if List.AllTrue(List.Transform(listDistinct, each Type.Is(_, type table))) then type nullable table
                else if List.AllTrue(List.Transform(listDistinct, each Type.Is(_, type list))) then type nullable list
                else if List.AllTrue(List.Transform(listDistinct, each Type.Is(_, type function))) then type nullable function
                /* // crazyMode is just for testing. Really, it just tries to assign the type found the most in the list. Don't use that.
                else if crazyMode??false then listMode */
                else if countDistinct = 2 then
                    if List.Contains(listDistinct, type text) then type text
                    else if List.ContainsAll(listDistinct, {type date, type datetime}) then type datetime
                    else if List.ContainsAll(listDistinct, {type date, type datetimezone}) then type datetimezone
                    else if List.ContainsAll(listDistinct, {type datetime, type datetimezone}) then type datetimezone
                    else if removeInts{0}? = type duration then type duration
                    else type any
                else if countDistinct = 3 then
                    if List.ContainsAll(listDistinct, {type datetimezone, type datetime, type date}) then type datetimezone
                    else if removeInts = {} then Int64.Type
                    else type any
                else  type any,

            // checkInt
            checkInt = (x as number) as type => if (Number.Mod(x, 1)??0) = 0 then Int64.Type else type number,
            // replaceWithType: custom replacer
            replaceWithType = (x,y,z) =>
                let
                    // nulls were a bother. Now Completely removed.
                    typ = if (x??"") = "" then type any else Value.Type(x)
                in
                    if Type.Is(typ, type number) then checkInt(x)
                    // next step is for excel and other sources which store time as '31/12/1899 hh:mm:ss'. Thankfully, pq also sees this as time if Time.From is used
                    else if Type.Is(typ, type datetime) then if Date.From(DateTime.From(x))=#date(1899,12,31) then type time else type datetime
                    else if Type.Is(typ, type text) then
                        if not (try Logical.FromText(x))[HasError] then type logical
                        else if not (try Number.FromText(x))[HasError] then checkInt(Number.From(x))
                        else if not (try Time.FromText(x))[HasError] then type time
                        else if not (try Date.FromText(x))[HasError] then type date
                        else if not (try DateTime.FromText(x))[HasError] then type datetime
                        else if not (try DateTimeZone.FromText(x))[HasError] then type datetimezone
                        else if not (try Duration.FromText(x))[HasError] then type duration
                        else type text
                    else typ,

            // typeToText
            typeToText = (typ as type, optional prim as logical) => let schema = Type.TableSchema(type table [a = typ]){0} in if prim??false then schema[Kind] else schema[TypeName],

            // Not allowed types
            typeList = { type null, type function, type list, type record, type table, type type },
            excludeTypes = (t as any) as logical => List.AllTrue(List.Transform(typeList, each not Type.Is(t, _))),

            // fakeFunction: takes a type and makes a function that returns this type, to be used for non appliable types
            // when Table.TransformColumnTypes is able to ascribe these 6 types then it will be redundant
            fakeFunction = (Type as type) as function =>
                if Type.Is(Type, type null) then (x) as null => null
                else if Type.Is(Type, type function) then (x) as nullable function => x
                else if Type.Is(Type, type list) then (x) as nullable list => x
                else if Type.Is(Type, type record) then (x) as nullable record => x
                else if Type.Is(Type, type table) then (x) as nullable table => x
                else if Type.Is(Type, type type) then (x) as nullable type => x
                else (x) => x,


            // Get types
            bufferTable = Table.Buffer(table),
            rowsToGet = List.Min({Table.RowCount(bufferTable), precision??500, 1000}),
            firstN = Table.FirstN((if columns = null then bufferTable else Table.SelectColumns(bufferTable, columns)), rowsToGet),
            columnNames = columns??Table.ColumnNames(firstN),

            tableValuesToType = Table.ReplaceValue(firstN, null, null, replaceWithType, columnNames),
            FindBestType = Table.CombineColumns(
                Table.Transpose(tableValuesToType),
                List.Transform({1..rowsToGet}, each "Column"&Text.From(_)),
                typeFinder,
                "Types"
                ),
            
            // Apply transformations
            allTransformations = List.Zip({columnNames, FindBestType[Types]}),
            applicableTransformations = List.Select(allTransformations,
                each excludeTypes( _{1} )),
            applySimpleTransformations = if applicableTransformations <> {} then Table.TransformColumnTypes(bufferTable, applicableTransformations) else bufferTable,
            
            applyAllList = List.Transform(List.RemoveItems(allTransformations, applicableTransformations), each {_{0}, fakeFunction(_{1}), _{1}}),
            applyOthers = if applyAllList <> {} then Table.TransformColumns(applySimpleTransformations, applyAllList) else applySimpleTransformations,

            textCode = let
                normalTypes = "Table.TransformColumnTypes( " & Expression.Identifier(tableName) & ", { "
                    & Text.Combine( List.Transform( applicableTransformations, each "{ """ & _{0} & """, " & typeToText(_{1}) & " }" ), ", " )
                    & " } )",
                abnormalTypes = Text.Combine( List.Transform( applyAllList, each  "{ """ & _{0} & """, (x) as nullable " & typeToText(_{2}, true) & " => x , type nullable " & typeToText(_{2}, true) & " }" ), ", " )
                    & " } )"
                in if applicableTransformations <> {} and applyAllList <> {} then
                        "let#(lf)#(tab)" & "normalTypes = " & normalTypes
                        & ",#(lf)#(tab)abnormalTypes = Table.TransformColumns( normalTypes, { " & abnormalTypes
                        & "#(lf)in#(lf)#(tab)abnormalTypes"
                    else if applicableTransformations <> {} then normalTypes
                    else if applyAllList <> {} then  "Table.TransformColumns( "& Expression.Identifier(tableName) &", { " & abnormalTypes
                    else null,

            Result = if tableName <> null then textCode else if applyOnlyNormalTypes??false then applySimpleTransformations else applyOthers
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
            optional applyOnlyNormalTypes as (type logical meta [
                Documentation.FieldCaption = "applyOnlyNormalTypes",
                Documentation.FieldDescription = "Switch to not do extra replacements for special types (e.g. type type). Default: <code>false</code>."
            ]),
            optional tableName as (type text meta [
                Documentation.FieldCaption = "tableName",
                Documentation.FieldDescription = "Give a table name in case of text return."
            ])
        )
        as any meta
        [
            Documentation.Name = "Table.InferColumnTypes",
            Documentation.Description = "Infers column types",
            Documentation.LongDescription = "Infers column types. Optional <code>precision</code>.<p>Supply a <code>tableName</code> to get back code as text.",
            Documentation.Category = "Table",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Infer table type",
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
	#""start"" = DateTime.Type,
	#""diff"" = Duration.Type,
	#""list"" = List.Type
]"
            ],[
                Description = "Infer table type",
                Code = "let
    startTable = #table(
        {""n"", ""fun"", ""rec"", ""tb"", ""l"", ""bin"", ""typ""},
        {{1, (x) => x, [a = 1], #table({""a""}, {{1}}), {1..3}, #binary(""""), type text}}
        )
in
    Table.InferColumnTypes ( StartTable )",
                Result = "type table [
	#""n"" = Int64.Type,
	#""fun"" = Function.Type,
	#""rec"" = Record.Type,
	#""tb"" = Table.Type,
	#""l"" = List.Type,
	#""bin"" = Binary.Type,
	#""typ"" = Type.Type
]"
            ],[
                Description = "get M code",
                Code = "let
    #""Start Table"" = #table(
        {""n"", ""fun"", ""rec"", ""tb"", ""l"", ""bin"", ""typ""},
        {{1, (x) => x, [a = 1], #table({""a""}, {{1}}), {1..3}, #binary(""""), type text}}
        )
in
    Table.InferColumnTypes ( #""Start Table"" , null, null, null, ""Start Table"")",
                Result = """let
	normalTypes = Table.TransformColumnTypes( #""Start Table"", { { ""n"", Int64.Type }, { ""bin"", Binary.Type } } ),
	abnormalTypes = Table.TransformColumns( normalTypes, { { ""fun"", (x) as nullable function => x , type nullable function }, { ""rec"", (x) as nullable record => x , type nullable record }, { ""tb"", (x) as nullable table => x , type nullable table }, { ""l"", (x) as nullable list => x , type nullable list }, { ""typ"", (x) as nullable type => x , type nullable type }, { ""tb typ"", (x) as nullable type => x , type nullable type } } )
in
	abnormalTypes"""
            ]}
        ]

in 
    Value.ReplaceType(TableInferColumnTypesFunction, TableInferColumnTypesType)