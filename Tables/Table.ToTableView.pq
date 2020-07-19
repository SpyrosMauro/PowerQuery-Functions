let
    // Function
    TableToTableViewFunction = (table as table, optional rowsToShow) as table =>
        let
            // addTabs: make text readable.
            addTabs = (t as text, optional tabsNumber as number) as text => Text.Replace ( t, "#(lf)", "#(lf)" & Text.Repeat("#(tab)", tabsNumber??1 ) ),


            // Calculations
            fixTableName = "#""" & "Real Fake Tables" & """",
            columnNames = Table.ColumnNames(table),

            findType =
                Table.SelectRows(
                    Table.Schema(table)[[Name], [Kind]],
                    each [Kind] <> "record" and [Kind] <> "table" and [Kind] <> "function" and [Kind] <> "binary" and [Kind] <> "list" and [Kind] <> "type"
                ),

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
                "Table.View (#(lf)#(tab)null,#(lf)#(tab)"
                & "[#(lf)#(tab)#(tab)" & addTabs(getType, 2)
                & ",#(lf)#(tab)#(tab)" & addTabs(getRows, 2)
                & ",#(lf)#(tab)#(tab)" & addTabs(onTake, 2) & "#(lf)#(tab)]" & "#(lf))",

            rec = [
                Table.View = Table.View,
                Table.FirstN = Table.FirstN,
                Table.RemoveColumns = Table.RemoveColumns,
                MissingField.Ignore = MissingField.Ignore,
                #"Real Fake Tables" = table
            ]
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
                ])
        )
        as table meta
        [
            Documentation.Name = "Table.ToTableView",
            Documentation.Description = "Generates <code>Table.View</code> to trick model asking for some rows",
            Documentation.LongDescription = "Executes <code>Table.View</code> to trick model asking for evaluation. Optional <code>rowsToShow</code>.<p>Creates <code>Table.View</code> based on <code>Table.Schema</code> and executes it.<p> <b>Skipping:</b> <i>Lists, Tables, Records, Binaries, Functions and Types</i><p><i>Only primitive types supported.</i>",
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