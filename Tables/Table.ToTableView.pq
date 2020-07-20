let
    // Function
    TableToTableViewFunction = (table as table, optional rowsToShow, optional primitive as logical) as table =>
        let
            // addTabs: make text readable.
            addTabs = (t as text, optional tabsNumber as number) as text => Text.Replace ( t, "#(lf)", "#(lf)" & Text.Repeat("#(tab)", tabsNumber??1 ) ),

            // Not allowed types
            typeList = 
                    if primitive??false then { "binary", "function", "list", "record", "table", "type" }
                    else { "Type.Binary", "Type.Function", "Type.List", "Type.Record", "Type.Table", "Type.Type" },
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
                if removeColumns
                    then "Table.RemoveColumns ( "& fixTableName & ", { " & Text.Combine(List.Transform(columnsToRemove, each """"&_&""""), ", ") & " } , MissingField.Ignore )"
                else fixTableName,

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
            if primitive??false then
                [
                    Table.View = Table.View,
                    Table.FirstN = Table.FirstN,
                    Table.RemoveColumns = Table.RemoveColumns,
                    MissingField.Ignore = MissingField.Ignore,
                    #"" = table
                ]
            else    
                Record.AddField(#shared, "", table)
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