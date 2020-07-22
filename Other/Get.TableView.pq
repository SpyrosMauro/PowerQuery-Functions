let
    // Function
    GetTableViewFunction = (table as table, tableName as text, optional precision as number, optional rowsToShow, optional primitive as logical, optional useOptimise as logical) as text =>
        let
            // addTabs: make text readable.
            addTabs = (t as text, optional tabsNumber as number) as text => Text.Replace ( t, "#(lf)", "#(lf)" & Text.Repeat("#(tab)", tabsNumber??1 ) ),

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

            // replaceWithType: custom replacer
            replaceWithType = (x,y,z) =>
                let
                    // nulls were a bother. Now Completely removed.
                    typ = if (x??"") = "" then type any else Value.Type(x)
                in
                    if Type.Is(typ, type number) then checkInt(x)
                    // next step is for excel and other sources which store time as '31/12/1899 hh:mm:ss'. Thankfully, pq also sees this as time if Time.From is used
                    else if Type.Is(typ, type datetime) then if not (try Time.From(x))[HasError] then type time else type datetime
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


            // Not allowed types
            typeList = 
                [
                    Text =
                        if primitive??false then { "binary", "function", "list", "record", "table", "type" }
                    else { "Binary.Type", "Function.Type", "List.Type", "Record.Type", "Table.Type", "Type.Type" },
                    Type = {type binary, type function, type list, type record, type table, type type, type [...]}
                ],
            excludeTypes = (t as any, textOrType as text) as logical =>
                if textOrType = "Text" then List.AllTrue(List.Transform(Record.Field(typeList, textOrType), each t <> _))
                else List.AllTrue(List.Transform(Record.Field(typeList, textOrType), each not Type.Is(t, _))),

            // typeToText: format types to #"Name" = type
            typeToText = 
                if primitive??false then (x as list) as text => "#""" & x{0} &"""" & " = ( type " & x{1} & " )"
                else (x as list) as text => "#""" & x{0} &"""" & " = " & x{1},

            // Schema columns
            schemaColumns = if primitive??false then {"Name", "Kind"} else {"Name", "TypeName"},

            // Calculations
            fixTableName = if Text.StartsWith(tableName, "#") then tableName else "#""" & tableName & """",
            columnNames = Table.ColumnNames(table),

            findType =
                if (precision??0) > 0 then
                    let
                        rowsToGet = List.Min({Table.RowCount(table), precision??50, 1000}),

                        tableValuesToType = Table.ReplaceValue(Table.FirstN(table, rowsToGet), null, null, replaceWithType, columnNames),
                        FindBestType = Table.CombineColumns(
                            Table.Transpose(tableValuesToType),
                            List.Transform({1..rowsToGet}, each "Column"&Text.From(_)),
                            typeFinder,
                            "Types"
                            ),

                        RemoveExtra = List.Select(
                            List.Zip({columnNames, List.Transform(FindBestType[Types], each if Type.Is(_, type null) then type any else _)}),
                            each excludeTypes(_{1}, "Type")),
                        realFakeTables = Table.TransformColumnTypes(#table(List.Transform(RemoveExtra, each _{0}), {}), RemoveExtra),
                        schema = Table.SelectColumns(Table.Schema(realFakeTables), schemaColumns)
                    in
                        schema
                else
                    Table.SelectRows(
                        Table.SelectColumns(Table.Schema(table), schemaColumns),
                        each excludeTypes(Record.Field(_, schemaColumns{1}), "Text") ),

            columnsToRemove = List.RemoveItems(columnNames, findType[Name]),
            removeColumns = List.Count(columnsToRemove) > 0,

            mainTableCall =
                let tbCall = 
                    if removeColumns
                        then "Table.RemoveColumns ( "& fixTableName & ", { " & Text.Combine(List.Transform(columnsToRemove, each """"&_&""""), ", ") & " } , MissingField.Ignore )"
                    else fixTableName
                in
                    if useOptimise??false then "Value.Optimize ( " & tbCall & " )" else tbCall,

            tableType =
                "type table [#(lf)#(tab)"
                & Text.Combine(Table.ToList(findType, typeToText ), ",#(lf)#(tab)")
                & "#(lf)]",
            
            getType = "GetType = () =>#(lf)#(tab)" & addTabs(tableType),

            getRows = "GetRows = () =>#(lf)#(tab)" & addTabs(mainTableCall),
            
            onTake =
                let
                    tbl = "#table (#(lf)#(tab)" & addTabs(tableType) & ",#(lf)#(tab){}#(lf))",
                    countText = if rowsToShow = null then "count" else Text.From(rowsToShow),
                    main =
                        if rowsToShow = 0 then tbl
                        else "if count = 0 then#(lf)#(tab)" & addTabs(tbl) & "#(lf)else Table.FirstN ( " & mainTableCall & ", " & countText & " )"
                in
                    "OnTake = (count as number) =>#(lf)#(tab)" & addTabs(main),


            tableView =
                "= Table.View (#(lf)#(tab)null,#(lf)#(tab)"
                & "[#(lf)#(tab)#(tab)" & addTabs(getType, 2)
                & ",#(lf)#(tab)#(tab)" & addTabs(getRows, 2)
                & ",#(lf)#(tab)#(tab)" & addTabs(onTake, 2) & "#(lf)#(tab)]" & "#(lf))"
        in
            tableView,


    // Documentation
    GetTableViewType =
        type function (
            table as (type table meta [
                Documentation.FieldCaption = "Target table",
                Documentation.FieldDescription = "The table for which to generate <code>Table.View</code>"
                ]),
            tableName as (type text meta [
                Documentation.FieldCaption = "Table name",
                Documentation.FieldDescription = "The name of the table in text"
                ]),
            optional precision as (type number meta [
                Documentation.FieldCaption = "precision",
                Documentation.FieldDescription = "First <code>n</code> rows to check for best type match"
                ]),
            optional rowsToShow as (type number meta [
                Documentation.FieldCaption = "rowsToShow",
                Documentation.FieldDescription = "Show first n rows when asking for 1000"
                ]),
            optional primitive as (type logical meta [
                Documentation.FieldCaption = "primitive",
                Documentation.FieldDescription = "Only primitive types"
                ]),
            optional useOptimise as (type logical meta [
                Documentation.FieldCaption = "useOptimise",
                Documentation.FieldDescription = "Use Value.Optimize"
                ])
        )
        as text meta
        [
            Documentation.Name = "Get.TableView",
            Documentation.Description = "Generates <code>Table.View</code> to trick model asking for evaluation",
            Documentation.LongDescription = "Generates <code>Table.View</code> to trick model asking for evaluation. Optional <code>precision</code> and <code>rowsToShow</code>.<p> <b>Skipping:</b> <i>Lists, Tables, Records, Binaries, Functions and Types</i>",
            Documentation.Category = "Model",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Generate table view of last step by infering type on 4 rows",
                Code = "let
    StartTable = Table.FromRecords({
        [start = #datetime(2020,7,1,0,0,0), diff = #duration(0,15,0,0), list = {1}],
        [start = #datetime(2020,7,1,5,0,0), diff = #duration(0,16,0,0), list = null],
        [start = #datetime(2020,7,1,8,0,0), diff = #duration(1,0,0,0), list = {4}],
        [start = #datetime(2020,7,1,16,0,0), diff = #duration(0,8,0,0), list = {3,4}]
    })
in
    Get.TableView ( StartTable , ""StartTable"", 4 )",
                Result = "= Table.View (
	null,
	[
		GetType = () =>
			type table [
				#""start"" = ( type datetime ),
				#""diff"" = ( type duration )
			],
		GetRows = () =>
			Table.RemoveColumns ( #""StartTable"", { ""list"" } , MissingField.Ignore ),
		OnTake = (count as number) =>
			if count = 0 then
				#table (
					type table [
						#""start"" = ( type datetime ),
						#""diff"" = ( type duration )
					],
					{}
				)
			else Table.FirstN ( Table.RemoveColumns ( #""StartTable"", { ""list"" } , MissingField.Ignore ), count )
	]
)"
            ],[
                Description = "Generate table view of last step",
                Code = "let
    StartTable = Table.FromRecords({
        [start = #datetime(2020,7,1,0,0,0), diff = #duration(0,15,0,0), list = {1}],
        [start = #datetime(2020,7,1,5,0,0), diff = #duration(0,16,0,0), list = null],
        [start = #datetime(2020,7,1,8,0,0), diff = #duration(1,0,0,0), list = {4}],
        [start = #datetime(2020,7,1,16,0,0), diff = #duration(0,8,0,0), list = {3,4}]
    })
in
    Get.TableView ( StartTable , ""StartTable"")",
                Result = "= Table.View (
	null,
	[
		GetType = () =>
			type table [
				#""start"" = ( type any ),
				#""diff"" = ( type any ),
				#""list"" = ( type any )
			],
		GetRows = () =>
			#""StartTable"",
		OnTake = (count as number) =>
			if count = 0 then
				#table (
					type table [
						#""start"" = ( type any ),
						#""diff"" = ( type any ),
						#""list"" = ( type any )
					],
					{}
				)
			else Table.FirstN ( #""StartTable"", count )
	]
)"
            ]}
        ]

in
    Value.ReplaceType(GetTableViewFunction, GetTableViewType)