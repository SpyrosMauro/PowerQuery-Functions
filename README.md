# powerquery
A collection of functions written in Power Query (M)
*Or "I paste stuff here sometimes"*


## What you'll find:

## Table Manipulation
| Function | Description |
| :--- | ----------- |
| [Table.CombineMultipleColumns](Tables/Table.CombineMultipleColumns.pq) | For the pain of excel files with double headers. Combine columns by any <code>setSize</code> in one step. |
| [Table.GenerateColumns](Tables/Table.GenerateColumns.pq) | To add multiple columns based on similar functions. *Have you thought about just unpivoting and adding one column afterwards?* |
| [Table.InferColumnTypes](Tables/Table.InferColumnTypes.pq) | Infer column types. **Only primitive types supported.** |
| [Table.ReplaceValuesFromTable](Tables/Table.ReplaceValuesFromTable.pq) | Optimised mass replace values in columns. |
| [Table.ToTableView](Tables/Table.ToTableView.pq) | Automatically apply a <code>Table.View</code> function based on <code>Table.Schema</code>. **Only primitive types supported.** |

## Time Intelligence
| Function | Description |
| :--- | ----------- |
| [DateTime.Diff](Time%20Intelligence/DateTime.Diff.pq) | DateTime Diff using <code>'Excluded Dates'</code> list and <code>'Start Time'</code> / <code>'End Time'</code>. |
| [Date.Diff](Time%20Intelligence/Date.Diff.pq) | Date Diff using <code>'Excluded Dates'</code> list. |
| [Duration.To](Time%20Intelligence/Duration.To.pq) | All Duration.Total**xxxx** functions in one. |

## Misc
| Function | Description |
| :--- | ----------- |
| [Get.TableView](Other/Get.TableView.pq) | Get formatted <code>Table.View</code> function based on <code>Table.Schema</code> or by infering types. **Only primitive types supported.** |
| [Types.TableType](Types/Type.TableType.pq) | Get text of non abstract table type. |