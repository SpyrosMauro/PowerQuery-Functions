let
    // Function
    TypeTableTypeFunction = (table as table, optional precision as number, optional primitive as logical) as any =>
        let
            // addTabs: make text readable.
            addTabs = (t as text, optional tabsNumber as number) as text => Text.Replace ( t, "#(lf)", "#(lf)" & Text.Repeat("#(tab)", tabsNumber??1 ) ),

            // typeToText: format types
            typeToText = 
                if primitive??false then (x as list) as text => if Text.StartsWith(x{1}, "type table [") then "#""" & x{0} &"""" & " = ( " & x{1} & " )" else "#""" & x{0} &"""" & " = ( type " & x{1} & " )"
                else (x as list) as text => "#""" & x{0} &"""" & " = " & x{1},

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
                    else if removeInts{0} = type duration then type duration
                    else type any
                else if countDistinct = 3 then
                    if List.ContainsAll(listDistinct, {type datetimezone, type datetime, type date}) then type datetimezone
                    else if removeInts = {} then Int64.Type
                    else type any
                else  type any,


            // Not allowed types
            typeList = 
                [
                    Text =
                        if primitive??false then { "null", "function", "list", "record", "table", "type" }
                        else { "Type.Null", "Type.Function", "Type.List", "Type.Record", "Type.Table", "Type.Type" },
                    Type = { type null, type function, type list, type record, type table, type type }
                ],
            excludeTypes = (t as any, textOrType as text) as logical =>
                if textOrType = "text" then List.AllTrue(List.Transform(Record.Field(typeList, textOrType), each t <> _))
                else List.AllTrue(List.Transform(Record.Field(typeList, textOrType), each not Type.Is(t, _))),

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

            // checkInt
            checkInt = (x as number) as type => if primitive??false then type number else if (Number.Mod(x, 1)??0) = 0 then Int64.Type else type number,
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

            tableTypeFromSchema = (schema as table) as text =>
                "type table [#(lf)#(tab)"
                & Text.Combine( Table.ToList( schema, (t) => addTabs(typeToText(t)) ), ",#(lf)#(tab)" )
                & "#(lf)]",


            // Schema columns
            schemaColumns = if primitive??false then {"Name", "Kind"} else {"Name", "TypeName"},

                    
            findType =
                if (precision??0) > 0 then
                    let
                        rowsToGet = List.Min({Table.RowCount(table), precision??50, 1000}),
                        firstN = Table.FirstN(table, rowsToGet),
                        columnNames = Table.ColumnNames(firstN),


                        tableValuesToType = Table.ReplaceValue(firstN, null, null, replaceWithType, columnNames),
                        FindBestType = Table.CombineColumns(
                            Table.Transpose(tableValuesToType),
                            List.Transform({1..rowsToGet}, each "Column"&Text.From(_)),
                            typeFinder,
                            "Types"
                            ),
                        
                        allTransformations = List.Zip({columnNames, List.Transform(FindBestType[Types], each if Type.Is(_, type null) then type any else _)}),
                        applicableTransformations = List.Select( allTransformations, each excludeTypes(_{1}, "Type")),
                        fakeTable = #table(List.Transform(allTransformations, each _{0}), {}),
                        applySimpleTransformations = if applicableTransformations <> {} then Table.TransformColumnTypes(fakeTable, applicableTransformations) else fakeTable,

                        notAllowedList = List.Transform(List.RemoveItems(allTransformations, applicableTransformations), each {_{0}, fakeFunction(_{1}), _{1}}),
                        checkExtraTypes = if notAllowedList <> {} then Table.TransformColumns(applySimpleTransformations, notAllowedList) else applySimpleTransformations,
                        schema = Table.SelectColumns(Table.Schema(checkExtraTypes), schemaColumns),
                        result =
                            if primitive??false
                                then Table.RemoveColumns(Table.AddColumn(schema, "Deep Dive",
                                    each if [Kind] = "table" then
                                        let name = [Name] in
                                        tableTypeFromSchema(Table.SelectColumns(Type.TableSchema(List.Select(notAllowedList, each _{0} = name){0}{2}), schemaColumns))
                                    else [Kind], type text)
                                    ,{"Kind"})
                                else schema
                    in
                        result
                else
                    Table.SelectColumns(Table.Schema(table), schemaColumns),

            tableType = tableTypeFromSchema(findType)
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
                ]),
            optional primitive as (type logical meta [
                Documentation.FieldCaption = "primitive",
                Documentation.FieldDescription = "Only primitive types"
                ])
        )
        as text meta
        [
            Documentation.Name = "Type.TableType",
            Documentation.Description = "Generates the text of non abstract table type",
            Documentation.LongDescription = "Generates <b>text of</b> <code>non abstract type table</code>. Optional <code>precision</code>.",
            Documentation.Category = "Type",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Table type to text",
                Code = "let
    StartTable = Table.FromRecords({
        [start = #datetime(2020,7,1,0,0,0), tb = #table(type table [a = number], {{1}}), list = {1}],
        [start = #datetime(2020,7,1,5,0,0), tb = #table(type table [a = number], {{2}}), list = null],
        [start = #datetime(2020,7,1,8,0,0), tb = #table(type table [a = number], {{3}}), list = {4}],
        [start = #datetime(2020,7,1,16,0,0), tb = null, list = {3,4}]
    })
in
    Type.TableType ( StartTable, 10, true )",
                Result = """type table [
	#""start"" = ( type datetime ),
	#""diff"" = ( type table [
    #(tab)#(tab)#""a"" = ( type any )
    #(tab)] ),
	#""list"" = ( type list )
]"""
            ],[
                Description = "Table type to text",
                Code = "let
    startTable = #table(
        {""n"", ""fun"", ""rec"", ""tb"", ""l"", ""bin"", ""typ""},
        {{1, (x) => x, [a = 1], #table({""a""}, {{1}}), {1..3}, #binary(""), type text}}
        )
in
    Type.TableType ( StartTable )",
                Result = """type table [
	#""n"" = Int64.Type,
	#""fun"" = Function.Type,
	#""rec"" = Record.Type,
	#""tb"" = Table.Type,
	#""l"" = List.Type,
	#""bin"" = Binary.Type,
	#""typ"" = Type.Type
]"""
            ]}
        ]

in
    Value.ReplaceType(TypeTableTypeFunction, TypeTableTypeType)