let
    // Function
    GenerateTableViewFunction = (table as table, tableName as text, optional precision as number, optional rowsToShow) as any =>
        let
            // addTabs: make text readable.
            addTabs = (t as text, optional tabsNumber as number) as text => Text.Replace ( t, "#(lf)", "#(lf)" & Text.Repeat("#(tab)", tabsNumber??1 ) ),

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
                    

            // Calculations
            fixTableName = if Text.StartsWith(tableName, "#") then tableName else "#""" & tableName & """",
            columnNames = Table.ColumnNames(table),

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

                        RemoveExtra = List.Select(
                            List.Zip({columnNames, List.Transform(FindBestType[Types], each if Type.Is(_, type null) then type any else _)}),
                            each _{1} <> type [...] and _{1} <> type record and _{1} <> type function and _{1} <> type binary and _{1} <> type table and _{1} <> type list and _{1} <> type type),
                        realFakeTables = Table.TransformColumnTypes(#table(List.Transform(RemoveExtra, each _{0}), {}), RemoveExtra),
                        schema = Table.Schema(realFakeTables)[[Name], [Kind]]
                    in
                        schema
                else
                    Table.SelectRows(Table.Schema(table)[[Name], [Kind]], each [Kind] <> "record" and [Kind] <> "table" and [Kind] <> "function" and [Kind] <> "binary" and [Kind] <> "list" and [Kind] <> "type"),

            columnsToRemove = List.RemoveItems(columnNames, findType[Name]),
            removeColumns = List.Count(columnsToRemove) > 0,

            mainTableCall =
                if removeColumns
                    then "Table.RemoveColumns ( "& fixTableName & ", { " & Text.Combine(List.Transform(columnsToRemove, each """"&_&""""), ", ") & " } , MissingField.Ignore )"
                else fixTableName,

            tableType =
                "type table [#(lf)#(tab)"
                & Text.Combine(Table.ToList(findType, (x) => "#""" & x{0} &"""" & " = ( type " & x{1} & " )" ), ",#(lf)#(tab)")
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
    GenerateTableViewType =
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
                ])
        )
        as text meta
        [
            Documentation.Name = "Generate.TableView",
            Documentation.Description = "Generates <code>Table.View</code> to trick model asking for some rows",
            Documentation.LongDescription = "Generates <code>Table.View</code> to trick model asking for evaluation. Optional <code>precision</code> and <code>rowsToShow</code>.<p> <b>Skipping:</b> <i>Lists, Tables, Records, Binaries, Functions and Types</i><p><i>Only primitive types supported.</i>",
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
    Generate.TableView ( StartTable , ""StartTable"", 4 )",
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
    Generate.TableView ( StartTable , ""StartTable"")",
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
    Value.ReplaceType(GenerateTableViewFunction, GenerateTableViewType)