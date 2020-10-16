let
    // Function
    TableToTableViewFunction = (table as table, optional rowsToShow, optional primitive as logical, optional useOptimise as logical) as table =>
        let
            // Record with Types
            allTypes = [
                Any.Type = Any.Type,
                None.Type = None.Type,
                Day.Type = Day.Type,
                Duration.Type = Duration.Type,
                Record.Type = Record.Type,
                Number.Type = Number.Type,
                Binary.Type = Binary.Type,
                Byte.Type = Byte.Type,
                Character.Type = Character.Type,
                Text.Type = Text.Type,
                Function.Type = Function.Type,
                Null.Type = Null.Type,
                List.Type = List.Type,
                Type.Type = Type.Type,
                Logical.Type = Logical.Type,
                Int8.Type = Int8.Type,
                Int16.Type = Int16.Type,
                Int32.Type = Int32.Type,
                Int64.Type = Int64.Type,
                Single.Type = Single.Type,
                Double.Type = Double.Type,
                Decimal.Type = Decimal.Type,
                Currency.Type = Currency.Type,
                Percentage.Type = Percentage.Type,
                Guid.Type = Guid.Type,
                Uri.Type = Uri.Type,
                Password.Type = Password.Type,
                Date.Type = Date.Type,
                DateTime.Type = DateTime.Type,
                DateTimeZone.Type = DateTimeZone.Type,
                Time.Type = Time.Type,
                Table.Type = Table.Type
            ],
            // addTabs: make text readable.
            addTabs = (t as text, optional tabsNumber as number) as text => Text.Replace ( t, "#(lf)", "#(lf)" & Text.Repeat("#(tab)", tabsNumber??1 ) ),

            // Not allowed types
            typeList = 
                    if primitive??false then { "binary", "function", "list", "record", "table", "type" }
                    else { "Binary.Type", "Function.Type", "List.Type", "Record.Type", "Table.Type", "Type.Type" },
            excludeTypes = (t as text) as logical => List.AllTrue(List.Transform(typeList, each t <> _)),

            typeToText = 
                if primitive??false then (x as list) as text => "#""" & x{0} &"""" & " = ( type " & x{1} & " )"
                else (x as list) as text => "#""" & x{0} &"""" & " = " & x{1},

            // Schema columns
            schemaColumns = if primitive??false then {"Name", "Kind"} else {"Name", "TypeName"},

            // Calculations
            fixTableName = "#""" & "" & """",

            schema = Table.SelectColumns(Table.Schema(table), schemaColumns),
            columnNames = schema[Name],

            findType = Table.SelectRows( schema, each excludeTypes( Record.Field(_, schemaColumns{1}) ) ),

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
                    & Text.Combine( Table.ToList( findType, typeToText ), ",#(lf)#(tab)" )
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
                "Table.View (#(lf)#(tab)null,#(lf)#(tab)"
                & "[#(lf)#(tab)#(tab)" & addTabs(getType, 2)
                & ",#(lf)#(tab)#(tab)" & addTabs(getRows, 2)
                & ",#(lf)#(tab)#(tab)" & addTabs(onTake, 2) & "#(lf)#(tab)]" & "#(lf))",

            rec = 
            Record.Combine({
                [
                    Table.View = Table.View,
                    Table.FirstN = Table.FirstN,
                    Table.RemoveColumns = Table.RemoveColumns,
                    MissingField.Ignore = MissingField.Ignore,
                    #"" = table
                ],
                allTypes
            })
        in
           Expression.Evaluate(tableView, rec),


    // Documentation
    TableToTableViewType =
        type function (
            table as (type table meta [
                Documentation.FieldCaption = "Target table",
                Documentation.FieldDescription = "The table for which to generate <code>Table.View</code>"
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
        as table meta
        [
            Documentation.Name = "Table.ToTableView",
            Documentation.Description = "Generates <code>Table.View</code> to trick model asking for evaluation",
            Documentation.LongDescription = "Executes <code>Table.View</code> to trick model asking for evaluation.<p>Creates <code>Table.View</code> based on <code>Table.Schema</code> and executes it.<p> <b>Skipping:</b> <i>Lists, Tables, Records, Binaries, Functions and Types</i>",
            Documentation.Category = "Table",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "For examples of how this code is generated see:",
                Code = "Get.TableView",
                Result = "Expression.Evaluate of generated table view." ]}
        ]

in
    Value.ReplaceType(TableToTableViewFunction, TableToTableViewType)