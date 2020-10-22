let
    // Function
    TableDistinctMergeFunction = (table1 as table, key1 as list, table2 as table, key2 as list, optional expandColumns as list, optional prefix as text, optional joinKind as number) as table =>
        let
            //Define combiner
            combiner =
            let
                fn = (x) => List.Zip( { x{0}??{}, x{1}??{} } )
            in
                if (joinKind??JoinKind.LeftOuter) = JoinKind.LeftOuter
                    then (x) => List.FirstN( fn(x), x{2} )
                else if joinKind = JoinKind.RightOuter
                    then (x) => List.FirstN( fn(x), x{3} )
                else if joinKind = JoinKind.Inner
                    then (x) => List.FirstN( fn(x), List.Min( { x{2}, x{3} } ) )
                else if joinKind = JoinKind.LeftAnti
                    then (x) => List.Select( fn(x), (y) => y{1}? = null )
                else if joinKind = JoinKind.RightAnti
                    then (x) => List.Select( fn(x), (y) => y{0}? = null )
                else fn,

            table1columns = Table.ColumnNames(table1),
            table2Columns = expandColumns??Table.ColumnNames(table2),
            newTable2Columns =
                if prefix = null then
                    let
                        names = List.Union({List.Intersect({key1, key2}), List.Intersect({table1columns, table2Columns})}),
                        fixNames = if names <> {}
                            then List.ReplaceMatchingItems(table2Columns, List.Transform(names, each {_, Text.Combine({_, ".1"})}))
                            else table2Columns
                    in
                        fixNames
                else List.Transform(table2Columns, each Text.Combine({prefix, ".", _})),
            joinKindfixed =
                if joinKind = JoinKind.LeftAnti or joinKind = null then JoinKind.LeftOuter
                else if joinKind = JoinKind.RightAnti then JoinKind.RightOuter
                else joinKind,


            GroupTable1 = Table.Group(table1, key1, {{"tableList1", Table.ToRecords, type list}, {"tableCount1", Table.RowCount, Int64.Type}}),
            GroupTable2 = Table.Group(Table.SelectColumns(table2, List.Union({table2Columns, key2})), key2, {{"tableList2", Table.ToRecords, type list}, {"tableCount2", Table.RowCount, Int64.Type}}),

            MergeTables = Table.ExpandTableColumn(Table.NestedJoin(GroupTable1, key1, GroupTable2, key2, "Data", joinKindfixed), "Data", {"tableList2", "tableCount2"}, {"tableList2", "tableCount2"}),

            MagicallyCombine = Table.CombineColumns(MergeTables,
                {"tableList1", "tableList2", "tableCount1", "tableCount2"},
                combiner,
                "Combined"),
            ExpandData =
                let
                    combinedColumn = 
                        let c = Table.ExpandListColumn(MagicallyCombine, "Combined")[#"Combined"]
                        in if (joinKind??JoinKind.LeftOuter) <> joinKindfixed then List.RemoveNulls(c) else c,
                    ExpandList = Table.FromList(combinedColumn, (x) => x, {"Table1 Data", "Table2 Data"}),
                    #"Table1 Data" = Table.ExpandRecordColumn(ExpandList, "Table1 Data", table1columns, table1columns)
                in
                    Table.ExpandRecordColumn(#"Table1 Data", "Table2 Data", table2Columns, newTable2Columns),

            fixTypes = 
                let
                    table1type = Value.Type(table1),
                    table2type = Value.Type(Table.RenameColumns(Table.SelectColumns(table2, table2Columns), List.Zip({table2Columns, newTable2Columns}))),
                    table1record = Record.FromList(List.Transform(table1columns, each [ Type = Type.TableColumn(table1type, _), Optional = false ]), table1columns),
                    table2record = Record.FromList(List.Transform(newTable2Columns, each [ Type = Type.TableColumn(table2type, _), Optional = false ]), newTable2Columns),

                    newTableType = type table Type.ForRecord(table1record & table2record, false)
                in
                    Value.ReplaceType(ExpandData, newTableType)
        in
            fixTypes,


    // Documentation
    TableDistinctMergeFromTableType = 
        type function (
            table1 as (type table meta [
                Documentation.FieldCaption ="Left side table",
                Documentation.FieldDescription = "The table on the left side of the merge"
                ]),
            key1 as (type {text} meta [
                Documentation.FieldCaption ="Left side key",
                Documentation.FieldDescription = "Columns use as key on the left table."
                ]),
            table2 as (type table meta [
                Documentation.FieldCaption ="Right side table",
                Documentation.FieldDescription = "The table on the left side of the merge"
                ]),
            key2 as (type {text} meta [
                Documentation.FieldCaption ="Right side key",
                Documentation.FieldDescription = "Columns use as key on the right table."
                ]),
            optional expandColumns as (type {text} meta [
                Documentation.FieldCaption ="Columns to expand",
                Documentation.FieldDescription = "The columns to expand after merge"
                ]),
            optional prefix as (type text meta [
                Documentation.FieldCaption ="prefix",
                Documentation.FieldDescription = "prefix to be used on new column names"
                ]),
            optional joinKind as (JoinKind.Type)
            )
        as table meta
        [
            Documentation.Name = "Table.DistinctMerge",
            Documentation.Description = "Combines two tables based on duplicate keys keeping only one item per row",
            Documentation.LongDescription = "To be used when trying to combine two tables where both sides have duplicates but one wouldn't like to assign more than one value to each row.<p><code>Table.DistinctMerge</code> takes each duplicate key and uses an injective function based solely on row positions:<p><i>Example:</i><p><table><tr><td>key | </td><td>table1 | </td><td>table2</td><td></td><td></td><td>=>  </td><td>key | </td><td>table1&amp;2</td></tr><tr><td>x</td><td>[...]</td><td>[...]</td><td></td><td></td><td>}  </td><td> x</td><td>[...] &amp; [...]</td></tr><tr><td>x</td><td>[...]</td><td>[...]</td><td></td><td></td><td>}  </td><td> x</td><td>[...] &amp; [...]</td></tr><tr><td>x</td><td>[...]</td><td>[...]</td><td></td><td></td><td>}  </td><td> x</td><td>[...] &amp; [...]</td></tr><tr><td>x</td><td></td><td>[...]</td><td></td><td></td><td></td><td></td><td></td></tr></table>",
            Documentation.Category = "Table",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Force a distinct merge of those two",
                Code = "let
    StartingTable =
        Table.FromRecords({
            [a = ""a"", b = 2],
            [a = ""b"", b = 2],
            [a = ""c"", b = 3],
            [a = ""d"", b = 3]
        }),

    OtherTable = 
        Table.FromRecords({
            [check = 2, ID = ""A""],
            [check = 2, ID = ""B""],
            [check = 3, ID = ""C""],
            [check = 3, ID = ""D""]
        })
in
    Table.DistinctMerge(StartingTable, {""b""}, OtherTable, {""check""})",
                Result = "Table.FromRecords({
            [a = ""a"", b = 2, check = 2, ID = ""A""],
            [a = ""b"", b = 2, check = 2, ID = ""B""],
            [a = ""c"", b = 3, check = 3, ID = ""C""],
            [a = ""d"", b = 3, check = 3, ID = ""D""]
        })"
                ]}
        ]

in
    Value.ReplaceType(TableDistinctMergeFunction, TableDistinctMergeFromTableType)